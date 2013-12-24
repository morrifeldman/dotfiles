require 'csv'
require 'rake/clean'
require 'action_view'
require 'simple_xlsx_reader'
require 'open3'
include ActionView::Helpers::NumberHelper

# current usage
# rake -g -j 5

#### Don't try and rely on the prerequisites because they don't work.
#### Many of the prerequisites are created immediately upon the
#### command starting.  Need to give this some thought.  One option is
#### just to touch a file at after running the main commands to signal
#### completion.  A more complicated but possibly more robust setup is
#### to create a wrapper that works with a database to log jobs and
#### completion.  The job description will list the files that should
#### be created by the job.  This list will be logged into a sqlite
#### file with a pending flag.  Upon completion of the job. The
#### pending flag is set to done.  When the rake task starts, first
#### all the pending files will be deleted.  An advantage of this is
#### that it could be

# Perhaps this could instead be implemented in clojure.  If I'm
# already keeping the list of jobs in a database

# Expect a sheet from SimpleXlsxReader with a header row and data rows
# Convert the header row to symbols and then return an array of hashes
# linking the header symbols to the data in each row.
def sheet2hash(sheet)
  headers = sheet.headers.map(&:to_sym)
  sheet.data.map do |row|
    Hash[headers.zip(row)]
  end
end

def add_ext(f,ext)
  f.chomp(ext) + '.' + ext
end

def id2log(id)
  add_ext(id,'log')
end

def id2sam(id)
  add_ext(id,'sam.gz')
end

def id2bam(id)
  add_ext(id,'bam')
end

count_dir = 'counts'
split_count_dir = 'split_counts'

id2count = lambda do |id|
  File.join(count_dir, add_ext(id,'tdf'))
end

id2split_count = lambda do |id|
  File.join(split_count_dir, add_ext(id, 'split.tdf'))
end

def id2count(id)
  add_ext(id,'tdf')
end

def id2split_count(id)
  add_ext(id,'split.tdf')
end

def id2tag_align(id)
  add_ext(id,'tagAlign.gz')
end

def id2phantom(id)
  add_ext(id,'tagAlign.pdf')
end

def id2peak(id, ip_dir)
  File.join(ip_dir, id + '_peaks.xls')
end

def append_to_log(log_file, log_data)
  File.open(log_file,'a') do |f|
    [*log_data].each {|line| f.puts line}
  end
end

# Runs the given command.  If it fails raise an error, but if not, log
# the command into the given log_file.
def run_and_log_cmd(cmd, log_file)
  puts cmd
  raise "Command: #{cmd} Failed" unless system(cmd)
  append_to_log(log_file,cmd)
end

# Run the given command.  If it fails raise an error, but if not, log the command and the command's output (std_out) into the given log file.
def run_and_log_output(cmd, log_file)
  puts cmd
  cmd_output = `#{cmd}`
  raise "Command: #{cmd} Failed" unless $?.success?
  append_to_log(log_file, cmd)
  append_to_log(log_file, cmd_output)
end

def run_and_log_err(cmd, log_file)
  puts cmd
  stdout,stderr,status = Open3.capture3(cmd)
  raise "Command: #{cmd} Failed" unless status.success?
  append_to_log(log_file, cmd)
  append_to_log(log_file, stderr)
end

def grep_last(search_file, search_string)
  `grep \'#{search_string}\' #{search_file}`.lines.to_a[-1]
end

def grep_last_split_first(search_file, search_string)
  grep_last(search_file, search_string).split[0]
end

#### get some input arguments
# expect sample_sheet.csv to define :sample_dir and :sample_id
# see below for other columns
# :sample_dir has fastq.gz files in it for :sample_id
sample_sheet_file = ENV['sample_sheet'] || 'sample_sheet.xlsx'

if File.exists?(sample_sheet_file)
  sample_sheet = sheet2hash(SimpleXlsxReader.open(sample_sheet_file).sheets.first)
else
  sample_sheet = [] # keeps the rest from getting executed
end

mapq = 10
n_proc = ENV['n_proc'] || 1
memory = ENV['memory'] || 750000000
limit = ENV['limit'] || false

#### First create the sam files
sample_sheet.each do |sample|
  sample_id = sample[:sample_id]
  sample_dir = sample[:sample_dir]
  sam_file = id2sam(sample_id)
  log_file = id2log(sample_id)
  file sam_file do
    limit_pipe = limit ?  "| head -n #{Integer(limit) * 4}" : ''
    gz_fastq = "gzip -dc #{File.join(sample_dir,'*.fastq.gz')} #{limit_pipe}"
    bowtie_cmd = "#{gz_fastq} | bowtie2 -p #{n_proc} -t -x hg19 -U - | gzip > #{sam_file}"
    # Note: the -mm option grinds to a halt when run on the debian
    # guest and windows host so limit this command with a -j setting
    # so that it doesn't run out of memory.  Each aligner needs 3.4g
    # of real memory.  For now, I'm Making this a simple task, we can
    # just rely on the n_proc command line setting, so that bowtie
    # will use many processors.  On the windows/debian VM it uses
    # almost all 8 cores with n_proc set to 8.
    run_and_log_err(bowtie_cmd, log_file)
  end
  task :bowtie_align => sam_file
end

#### Now Bam files
sample_sheet.each do |sample|
  sample_id = sample[:sample_id]
  log_file = id2log(sample_id)
  sam_file = id2sam(sample_id)
  bam_file = id2bam(sample_id)
  file bam_file => [sam_file] do
    #puts "Counting the alignments for #{sample_id} ..."
    #number_alignments = Integer(`gzip -dc #{sam_file} | samtools view -Sc -`)
    # count them out of the bowtie_align log instead
    # the line we want looks like this '24397387 reads; of these:'
    number_alignments = Integer(grep_last_split_first(log_file, ' reads; of these:$'))
    puts "Filtering and sorting the alignments for #{sample_id}"
    samtools_cmd = "gzip -dc #{sam_file} | samtools view -q #{mapq} -Su - | samtools rmdup -s - - | samtools sort -m #{memory} - #{sample_id}"
    # view parameters:
    # -q for the mapq cuttof
    # -S means the input is Sam
    # -u means to output uncompressed bam
    # rmdup parameters
    # -s single-end reads
    # - - read from std-in, write to std-out
    run_and_log_err(samtools_cmd, log_file)
    puts "Counting the good alignments for #{sample_id} ..."
    number_good_alignments = Integer(`samtools view -c #{bam_file}`)
    percent_aligned = (100 * (number_good_alignments.to_f / number_alignments)).round
    log = []
    log << "#{sample_id} Filtered with MAPQ < #{mapq} and Removed Duplicates" <<
      "#{number_with_delimiter(number_alignments)} Total Alignments Before Filtering" <<
      "#{number_with_delimiter(number_good_alignments)} Good Alignments After Filtering" <<
      "#{percent_aligned}% Good Alignments After Filtering"
    append_to_log(log_file,log)
    puts "Indexing #{bam_file} ..."
    system("samtools index #{bam_file}")
  end
  multitask :filter_alignments => bam_file
end

##### now tagAlign files
sample_sheet.each do |sample|
  sample_id = sample[:sample_id]
  bam_file = id2bam(sample_id)
  tag_align_file = id2tag_align(sample_id)
  log_file = id2log(sample_id)
  file tag_align_file => [bam_file] do
    puts tag_align_command = %Q[samtools view -F 0x0204 -o - #{bam_file} | awk 'BEGIN{OFS="\t"}{if (and($2,16) > 0) {print $3,($4-1),($4-1+length($10)),"N","1000","-"} else {print $3,($4-1),($4-1+length($10)),"N","1000","+"} }' | gzip -c > #{tag_align_file}]
    raise "Command: #{tag_align_command} failed" unless system(tag_align_command)
    append_to_log(log_file, tag_align_command)
  end
  multitask :tag_align => tag_align_file
end

##### now make quality control graphs with run_spp.R
sample_sheet.each do |sample|
  sample_id = sample[:sample_id]
  tag_align_file = id2tag_align(sample_id)
  log_file = id2log(sample_id)
  phantom_pdf_file = id2phantom(sample_id)
  file phantom_pdf_file => tag_align_file do
    run_and_log_output("run_spp.R -s=-100:5:600 -c=#{tag_align_file} -savp -out=#{log_file} -rf", log_file)
  end
  multitask :phantom => phantom_pdf_file
end

# after looking at the coverage files, define extFactor in the sample_sheet
##### Now tdf coverage files
directory count_dir
multitask :raw_coverage => count_dir
directory split_count_dir
multitask :split_coverage => split_count_dir

sample_sheet.each do |sample|
  sample_id = sample[:sample_id]
  ext_factor = sample[:ext_factor]
  log_file = id2log(sample_id)
  bam_file = id2bam(sample_id)
  count_file = id2count.(sample_id)
  split_count_file = id2split_count.(sample_id)
  file count_file => [bam_file] do
    raise "Please define ext_factor in sample_sheet.csv for #{sample_id}, Suggestion: fragment length minus read length" unless ext_factor.to_i
    igv_count_command = "igvtools count --extFactor #{ext_factor} #{bam_file} #{count_file} hg19"
    run_and_log_cmd(igv_count_command, log_file)
  end
  file split_count_file => [bam_file] do
    # Not using extFactor here.  I think it would negate the shifting
    # effect I'm looking to see where I expect that the + and - reads
    # are offset
    split_count_cmd = "igvtools count --strands read #{bam_file} #{split_count_file} hg19"
    run_and_log_cmd(split_count_cmd, log_file)
  end
  multitask :raw_coverage => count_file
  multitask :split_coverage => split_count_file
end


# for downstream we should go ahead a sort with samtools before making
# the bam file. perhaps also index it! and then use igv tools to make
# a tdf file

# now setup to do peak finding for all the data
# sample sheet has columns exp, mark, group, treat, control

### I should probably add --SPMR to this command and rerun
def make_macs2_cmd(dir, treat_bams, control_bams, name, shiftsize)
  "cd #{dir} && macs2 callpeak -t #{[*treat_bams].join(' ')} -c #{[*control_bams].join(' ')} -f BAM -g hs -n #{name} -B --nomodel --shiftsize #{shiftsize}"
end

### I should add a task for making enrichment tracks
def name_dir(name, dir)
  File.join(dir, name)
end

def treat_pileup(name, dir)
  name_dir(name, dir) +  '_treat_pileup.bdg'
end

def control_lambda(name, dir)
  name_dir(name, dir) + '_control_lambda.bdg'
end

def fold_enrich_bdg(name, dir)
  name_dir(name, dir) + '_FE.bdg'
end

def subtract_bdg(name, dir)
  name_dir(name, dir) + '_subtract.bdg'
end

def make_macs2_bdg_base_cmd(pileup, lambda, name, dir)
  "macs2 bdgcmp -t #{pileup} -c #{lambda} -o #{name_dir(name, dir)}"
end

def make_macs2_fe_cmd(pileup, lambda, name, dir)
  make_macs2_bdg_base_cmd(pileup, lambda, name, dir) + " -m FE"
end

def make_macs2_subtract_cmd(pileup, lambda, name, dir)
  make_macs2_bdg_base_cmd(pileup, lambda, name, dir) +" -m subtract"
end

# convert the .bdg -> bigwig for example see https://gist.github.com/taoliu/2469050
# bdg2bw bdg-file ~/bin/hg19.chrom.lengths

def switch_ext(f, ext)
  f.chomp(File.extname(f)) + ext
end

def bdg2bw_subtask(bdg_file, log_file, task_sym)
  bdg2bw_cmd = "bdg2bw #{bdg_file} ~/bin/hg19.chrom.lengths"
  bw_file = switch_ext(bdg_file, '.bw')
  file switch_ext(bdg_file, '.bw') => bdg_file do
    run_and_log_err(bdg2bw_cmd, log_file)
  end
  task task_sym => bw_file
end

def macs_bdgcmp_tasks(name, dir, log_file, task_sym)
  pileup = treat_pileup(name, dir)
  lambda = control_lambda(name, dir)
  to_bw_task_sym = (task_sym.to_s + '_to_bw').to_sym

  bdg2bw_subtask(lambda, log_file, to_bw_task_sym)

  fe_bdg = fold_enrich_bdg(name, dir)
  file fe_bdg => [pileup, lambda] do
    fe_bdg_cmd = make_macs2_fe_cmd(pileup, lambda, name, dir)
    run_and_log_err(fe_bdg_cmd, log_file)
  end
  task task_sym => fe_bdg

  bdg2bw_subtask(fe_bdg, log_file, to_bw_task_sym)

  sub_bdg = subtract_bdg(name, dir)
  file sub_bdg => [pileup, lambda] do
    sub_bdg_cmd = make_macs2_subtract_cmd(pileup, lambda, name, dir)
    run_and_log_err(sub_bdg_cmd, log_file)
  end
  task task_sym => sub_bdg

  bdg2bw_subtask(sub_bdg, log_file, to_bw_task_sym)

end

sample_sheet.each do |sample|
  ip_dir = sample[:ip]
  next if ip_dir == 'input'
  sample_id = sample[:sample_id]
  shiftsize = sample[:shiftsize].to_i
  directory ip_dir
  multitask :macs2_peak => ip_dir
  log_file = id2log(sample_id)
  treat_bam_file = File.expand_path(id2bam(sample_id))
  control_bam_file = File.expand_path(id2bam(sample[:control]))
  peak_file = id2peak(sample_id,ip_dir)
  file peak_file => [treat_bam_file, control_bam_file] do
    raise "Please define shiftsize in sample sheet for #{sample_id}.  Suggestion: fragment length / 2" unless shiftsize
    # cding into the directory seems a bit of a hack, but Dir.chdir is
    # not thread safe!  but it is nice that the cd goes into the log
    # so we can know where the files will end up
    macs_cmd = make_macs2_cmd(ip_dir, treat_bam_file,
                              control_bam_file, sample_id, shiftsize)
    run_and_log_err(macs_cmd, log_file)
  end
  multitask :macs2_peak => peak_file

  macs_bdgcmp_tasks(sample_id, ip_dir, log_file, :macs2_bdgcmp)

end


# Now combine the replicates and make new bedgraph files, do peak
# calling on the combined data see [Signal Track from
# MACS2](https://github.com/taoliu/MACS/wiki/Build-Signal-Track).  Use
# the group column from the sample_sheet to decide which data to pool.

# Do this with both normal and broad peak calling

# parse the sample sheet into a hash {:group {:treat [treats] :control
# [controls] :shiftsize x}} use average shiftsize between the treated
# data.

# Get a list of groups
groups = sample_sheet.collect do |sample|
  sample[:group]
end.uniq

# Initalize the hash for group info
rep_hash = {}
groups.each do |group|
  rep_hash[group] = {treats: [], controls: [], shiftsize: []}
end

# To Do:
# Here I could calculate correlation between the replicates

# Fill the has containing group info
sample_sheet.each do |sample|
  ip = sample[:ip]
  group = sample[:group]
  if ip == 'input'
    rep_hash.delete(group)
    groups.delete(group)
    next
  end
  sample_id = sample[:sample_id]
  treat_bam_file = id2bam(sample_id)
  control_bam_file = id2bam(sample[:control])
  rep_hash[group][:treats] << treat_bam_file
  rep_hash[group][:controls] << control_bam_file
  rep_hash[group][:shiftsize] << sample[:shiftsize].to_i
  rep_hash[group][:output_dir] = ip # Just take the last :ip
end

# puts rep_hash
# Average the shift sizes
groups.each do |group|
  ss_array = rep_hash[group][:shiftsize]
  rep_hash[group][:shiftsize] = (ss_array.inject(:+).to_f / ss_array.size).to_i
end

# Walk through the groups.  Make combined and combined broad peaks
# This section is now really long
groups.each do |group|
  # Common Variables
  rep_info = rep_hash[group]
  shiftsize = rep_info[:shiftsize]
  log_file = id2log(group)
  treat_bam_files = rep_info[:treats].map{ |f| File.expand_path(f)}
  control_bam_files = rep_info[:controls].map{ |f| File.expand_path(f)}

  # Regular peak calling
  comb_task = :macs2_comb_peak
  comb_output_dir = rep_info[:output_dir] + '_comb'
  directory comb_output_dir
  multitask comb_task => comb_output_dir
  peak_file = id2peak(group,comb_output_dir)

  file peak_file => rep_info[:treats] + rep_info[:controls] do
    macs_cmd = make_macs2_cmd(comb_output_dir, treat_bam_files,
                              control_bam_files, group, shiftsize)
    run_and_log_err(macs_cmd, log_file)
  end
  multitask comb_task => peak_file

  # comb bdgcmp tracks
  macs_bdgcmp_tasks(group, comb_output_dir,
                    log_file, :macs2_comb_bdgcmp)

  # Broad peak calling
  broad_comb_task = :macs2_broad_comb_peak
  broad_comb_output_dir = rep_info[:output_dir] + '_broad_comb'
  directory broad_comb_output_dir
  multitask broad_comb_task => broad_comb_output_dir
  peak_file = id2peak(group,broad_comb_output_dir)

  file peak_file => rep_info[:treats] + rep_info[:controls] do
    macs_cmd = make_macs2_cmd(broad_comb_output_dir, treat_bam_files,
                              control_bam_files, group, shiftsize) + ' --broad'
    run_and_log_err(macs_cmd, log_file)
  end
  multitask broad_comb_task => peak_file

  # broad bdgcmp tracks
  macs_bdgcmp_tasks(group, broad_comb_output_dir,
                    log_file, :macs2_broad_comb_bdgcmp)

end

### now pull the quality metrics out of the log files and save them as
### a csv

task 'phantom_meta.log' do |f|
  File.open(f.name,'w') do |fd|
    fd << %w[
filename
numReads
estFragLen
corr_estFragLen
phantomPeak
corr_phantomPeak
argmin_corr
min_corr
nsc
rsc
qualityTag
].join("\t") << "\n"
    sample_sheet.each do |sample|
      sample_id = sample[:sample_id]
      log_file = id2log(sample_id)
      fd << grep_last(log_file, '.tagAlign.gz\t').gsub(/,[^\t]+/,'')
    end
  end
  end


task 'alignment_meta.log.csv' do |f|
  CSV.open(f.name,'w') do |fd|
    fd << %w[
sample_id
total_reads
unaligned
uniquely_aligned
not_uniq_align
filt_align
percent_good_align
]
    sample_sheet.each do |sample|
      sample_id = sample[:sample_id]
      log_file = id2log(sample_id)
      total_reads = Integer(grep_last_split_first(log_file, ' reads; of these:$'))
      unaligned = grep_last_split_first(log_file, ' aligned 0 times$')
      uniquely_aligned = grep_last_split_first(log_file, ' aligned exactly 1 time$')
      not_uniq_align = grep_last_split_first(log_file, ' aligned >1 times$')
      filt_align = grep_last_split_first(log_file, '[^%] Good Alignments After Filtering$').gsub(',','')
      percent_good_align = 100 * filt_align.to_i / total_reads.to_f
      percent_good_string = "%0.1f" % percent_good_align
      fd << [sample_id, total_reads, unaligned,
             uniquely_aligned, not_uniq_align, filt_align, percent_good_string]
    end
  end
end


CLOBBER.include('*.sam', '*.bam', '*.bai', '*.tdf', '*.sam', '*.log')

### to make this script robust to hard stops, run_cmd should take a
### list of files that will be created by the process, and put these
### in a list of files to be deleted before starting up again.
### Alternately it could just keep the list and delete them if the
### exit status is not success? but this would not work in the case of
### a hard reboot.  Essentially, I'm reinventing a journal here, so
### perhaps I can find a pre-built way to do it.  but if I can't then
### I can keep a list of pending files and delete them before starting
### up again.  Then after a command runs, the files should get
### removed. The difficulty is that many threads will need access to
### list of files, so I will need to lock the file while one thread
### removes it's files, or create a bunch of xxx.pending files to
### signal that xxx is not finished.  This won't work for the sorting
### command although these can perhaps be fixed by using a wild card
### durring the deletion something like xxxx.*bam.  To get around the
### locking problem, I can use a sqlite database to keep track of the
### pending files.  It will then handle the locking issues for me.
### Something like insert XXX and then after the command runs, do a
### delete where cmd = xxx.  The table could have columns like cmd,
### file_list, time_stamp.  file_list could incoporate wildcards.  cmd
### could be the actual command or something shorter.  The actual
### command might be problematic because of escaping issues.  Better
### could just be to skip the command all together and just list the
### files.  After the insert, get the row id and then delete by rowid.
### or even better, just have a pending column and set it to false for
### the rowid actually we don't need to care so much about stray
### files.  They should get overwritten the next time the command
### runs, we just need to make sure that the file the rake "file" task
### is keyed on will get deleted in the event of premature stoppage.
# e.g.
#r = nil
#db.transaction do |db|
#db.execute("insert into insert_test values ('t','h')")
#a = db.last_insert_row_id; end


#puts "Deleting #{sam_file}"
#system("rm #{sam_file}")
