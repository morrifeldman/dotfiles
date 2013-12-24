load './microarray_config.rb'

# microarray_config in the current directory should define the
# following constants.  

# Probe_category e.g. 'comprehensive' 

# Data_name, name of the analysis Conditions, if there are replicates,
# these are the base names of the replicates.  

# Control_name, this is one of the conditions 

# Orig_cel_dir, where to copy the cel files from 

# Cel_dir, where to copy the renamed cel files to 

# Threshold, a log2 threshold for some of the analyses 

# Orig_cel_names, the names of the cel files in order to match up with
# either the replicates and conditions or with Cel_names, see below.

# optional
# GSE, If downloading a GSE.

## either:
# N_reps, if the data is neatly organized into replicates
# otherwise
# Cel_names

def prefix_files(prefix, file_array)
  file_array.map{|f| File.join(prefix, f)}
end

# If your data don't have the same number of reps for each condition, define Cel_names directly
if defined?(N_reps)
  cel_files = Conditions.inject([]) do |acc, cond|
    N_reps.times {|i| acc << File.join(Cel_dir, "#{cond}_#{i+1}")}
    acc
  end
  else
  raise "Cel_files is deprecated in favor of Cel_names and Cel_dir" unless Cel_names
  cel_files = prefix_files(Cel_dir, Cel_names)
end

orig_cel_files = prefix_files(Orig_cel_dir, Orig_cel_names)

require 'rake/clean'
require 'mar_subs'

CLOBBER.include('ps','mps')

file_hash = Hash[orig_cel_files.zip(cel_files)]

######################## Get the GSE from NCBI
if defined? GSE
  gse_url = "http://www.ncbi.nlm.nih.gov/geosuppl/?acc=#{GSE}"
  
  directory Orig_cel_dir
  tar_file = File.join(Orig_cel_dir, "#{GSE}.tar")
  file tar_file => Orig_cel_dir do |f|
    sh "curl -o #{f.name} #{gse_url}"
  end
  #CLOBBER.include(Orig_cel_dir, tar_file) uncomment to clobber
  # the the downloaded tar file
  
  
  # the m switch tells tar to give the extracted
  # data a current timestamp.  This will keep the cel_files
  # newer than the tar file.  Otherwise they keep getting downloaded
  orig_cel_files.map{|cel| "#{cel}.gz"}.each do |cel_gz|
    file cel_gz => tar_file do |f|
      sh "tar -mxvf #{f.prerequisites[0]} -C #{Orig_cel_dir}"
    end
  end
  
  orig_cel_files.each do |cel|
    file cel => "#{cel}.gz" do |f|
      sh "gunzip -cv #{f.prerequisites[0]} > #{f.name}"
    end
  end
  
  CLOBBER.include(File.join(Orig_cel_dir, '*.gz'))
  CLOBBER.include(File.join(Orig_cel_dir, '*.CEL'))
else
  # put some code in here to fetch/unpack your data if needed
end

######################## Copy Files
# Why can't I just soft link here?  It doesn't work with APT.
directory Cel_dir
CLOBBER.include(Cel_dir)
file_hash.each do |orig_name, new_name|
  file new_name => [orig_name, Cel_dir] do |f|
    puts "copying #{f.prerequisites[0]} to #{f.name}"
    cp f.prerequisites[0], f.name
  end
  CLOBBER.include(new_name)
end

# ######################## link Files
# # Why can't I just soft link here?  It doesn't seem to APT, but I'm not exactly sure why.
# directory Cel_dir
# CLOBBER.include(Cel_dir)
# file_hash.each do |orig_name, new_name|
#   file new_name => [orig_name, Cel_dir] do |f|
#     puts "linking #{f.prerequisites[0]} to #{f.name}"
#     ln_s(File.join('..', f.prerequisites[0]), f.name)
#   end
#   CLOBBER.include(new_name)
# end

######################### Create cel_files.txt
cel_file_txt = 'cel_files.txt'
file 'cel_files.txt' => cel_files do
  File.open(cel_file_txt,'w') do |io|
    io << "cel_files\n"
    io << cel_files.sort.join("\n")
  end
end
CLOBBER.include(cel_file_txt)

######### Configuration for APT, requires AFFX_ANALYSIS_FILES_PATH to be set
celDir = File.absolute_path('.')
celFiles = File.join(celDir,'*.CEL')
libBase = 'HuEx-1_0-st-v2.r2'
pgf = libBase + '.pgf'
clf = libBase + '.clf'
qcc = libBase + '.qcc'
bgp = libBase + '.antigenomic.bgp'
apt = 'apt-probeset-summarize'
action = 'rma-sketch'

############################## APT Summarize
summary_proc = ->(ps_or_mps) {"#{ps_or_mps}/rma-sketch.summary.txt"}
ps_mps_task( summary_proc, cel_files + [cel_file_txt]) do |ps_or_mps|
    ps_or_mps_flag = ps_or_mps == 'ps' ? "-s" : "-m"
    ps_or_mps_lib = "#{libBase}.dt1.hg18.#{Probe_category}.#{ps_or_mps}"
    cmd = "#{apt} -a #{action} -p #{pgf} -c #{clf} #{ps_or_mps_flag} #{ps_or_mps_lib} -b #{bgp} -o #{ps_or_mps} --qc-probesets #{qcc} --cel-files #{cel_file_txt}"
    sh cmd
end

############################### RemoveAffyHeader
expr_proc = ->(ps_or_mps) {"#{ps_or_mps}/expr.#{ps_or_mps}"}
ps_mps_task(expr_proc, summary_proc) do |ps_or_mps, f|
  puts "Striping Affy Header"
  sh "removeAffyHeader.rb -f #{f.prerequisites[0]} -o #{f.name}"
end
expand_id(expr_proc)

############################## Average Replicates
rep_average_proc = ->(ps_or_mps) {"#{ps_or_mps}/rep_average.#{ps_or_mps}"}
ps_mps_task(rep_average_proc, expr_proc) do |ps_or_mps, f|
  average_replicates(f.prerequisites[0], f.name)
end
expand_id( rep_average_proc)

greatly_expand_id(rep_average_proc)

############################## Make condition specific bedfiles
# only do this for ps

def cond_bed_name(cond)
  "ps/#{cond}.bed"
end

Conditions.each do |cond|
  cond_bed = cond_bed_name(cond)
  file cond_bed => rep_average_proc.('ps') do |f|
    track_name = "raw_#{cond}"
    track_desc = "Raw Expression Values for #{cond}, #{Probe_category} probesets"
    track_opts = {track_name: track_name,
      track_desc: track_desc}
    bed_from_ps(f.prerequisites[0], cond, f.name, track_opts)
    sh "gzip -cv #{f.name} > #{f.name}.gz"
  end
end


######################## Combine the condition specific bed files and compress
multi_bed = "ps/raw_multi.bed"
all_cond_beds = Conditions.map{|cond| cond_bed_name(cond)}
file multi_bed => all_cond_beds do |f|
  f.prerequisites.each do |bed_file|
    sh "cat #{bed_file} >> #{f.name}"
  end
end

multi_bed_gz = "ps/raw_multi.bed.gz"
file multi_bed_gz => multi_bed do |f|
  sh "gzip -cv #{f.prerequisites[0]} > #{f.name}"
end
task raw_bed: multi_bed_gz

# don't do this task by default


########################### Make mean bed file across all datapoints
# only do this for ps
mean_bed = "ps/all_tp_mean.bed"
  # we average the raw data, because there might be a different number
  # of replicates in each condition
file mean_bed => expr_proc.('ps') do |f|
    track_name = "raw_tp_mean"
    track_desc = "Raw Expression Values Averaged Across All Time Points, #{Probe_category}"
    make_average_bed(f.prerequisites[0], f.name, track_name, track_desc)
end

mean_bed_gz = "ps/all_tp_mean.bed.gz"
file mean_bed_gz => mean_bed do |f|
  sh "gzip -cv #{f.prerequisites[0]} > #{f.name}"
end
task microarray: mean_bed_gz

#################### Normalize data

data_normalizer = make_data_normalizer(Control_name)
#this is a proc/lambda
norm_avg_proc = ->(ps_or_mps) {"#{ps_or_mps}/norm_avg.#{ps_or_mps}"}
ps_mps_task(norm_avg_proc, rep_average_proc) do |ps_or_mps, f|
  data_normalizer.(f.prerequisites[0], f.name)
end
expand_id(norm_avg_proc)


################## Make condition specific bed file from Norm Data
# only do this for ps
# don't do it for control, only for treated
treatments = Conditions.reject{|e| e == Control_name}

def norm_cond_bed_name(cond)
  "ps/norm_#{cond}.bed"
end

treatments.each do |cond|
  bed_name = norm_cond_bed_name(cond)
  file bed_name => norm_avg_proc.('ps') do |f|
    track_name = "norm_#{cond}"
    track_desc = "Normalized Expression Values for #{cond}, #{Probe_category} probesets"
    track_opts = {track_name: track_name,
      track_desc: track_desc,
      autoScale: 'off',
      viewLimits: '-1:1',
      yLine: 'on'}
    bed_from_ps(f.prerequisites[0], cond, f.name, track_opts)
    sh "gzip -cv #{f.name} > #{f.name}.gz"
  end
end

######################## Combine the norm condition specific bed files and compress
norm_multi_bed = "ps/norm_multi.bed"
all_norm_cond_beds = treatments.map{|cond| norm_cond_bed_name(cond)}
file norm_multi_bed => all_norm_cond_beds do |f|
  f.prerequisites.each do |bed_file|
    sh "cat #{bed_file} >> #{f.name}"
  end
end

norm_multi_bed_gz = "ps/norm_multi.bed.gz"
file norm_multi_bed_gz => norm_multi_bed do |f|
  sh "gzip -cv #{f.prerequisites[0]} > #{f.name}"
end
task norm_bed: norm_multi_bed_gz

#don't automatically do this task


##################### 2-fold up and down
treatments.each do |treat|
  [Threshold, -Threshold].each do |thresh|
  
    up_down_proc = ->(ps_or_mps) do
      up_or_down = (thresh > 0) ? 'up' : 'down'
      "#{ps_or_mps}/#{treat}_#{up_or_down}.#{ps_or_mps}"
    end
    
    ps_mps_task(up_down_proc, norm_avg_proc) do |ps_or_mps, f|
      thresh_filter = make_col_thresh_filter(thresh)
      filt_data = apply_proc_with_col(f.prerequisites[0], treat, &thresh_filter)
      sorter = make_col_sorter(thresh)
      sort_data = apply_proc_with_col(filt_data, treat, &sorter)
      File.write(f.name, sort_data.join)
    end
    expand_id(up_down_proc)
  end
end

############### Make fire input files
treatments.each do |treat|
  treat_tc = "mps/#{treat}.mps"
  file treat_tc => norm_avg_proc.('mps') do |f|
    extract_data_col(f.prerequisites[0], treat, f.name)
  end
  treat_ref_seq = "mps/#{treat}.ref_seq"
  file treat_ref_seq => treat_tc do |f|
    sh "geneConvert.rb -h -c tc2refSeq -f #{f.prerequisites[0]} -o #{f.name}"
  end
  treat_fire_in = "mps/#{treat}.fire_in"
  file treat_fire_in => treat_ref_seq do |f|
    sh "makeUniqueId.rb #{f.prerequisites[0]} #{f.name}"
  end
  treat_fire_in8 = "#{treat_fire_in}8"
  file treat_fire_in8 => treat_fire_in do |f|
    cp f.prerequisites[0], f.name
  end
  task microarray: treat_fire_in8
  fire_dir = '/Volumes/Spin/src/FIRE-1.1a'

  {treat_fire_in => 7, treat_fire_in8 => 8}.each do |fire_in, k|
    fire_out_dir = "#{fire_in}_FIRE"
    file fire_out_dir => fire_in do |f|
      sh "cd #{fire_dir} && perl fire.pl --expfile #{File.expand_path(f.prerequisites[0])} --species=human --exptype=continuous -k=#{k}"
    end
  end
end

###################### 2-fold up and down across all treatments
[Threshold, -Threshold].each do |thresh|
  all_up_down_proc = ->(ps_or_mps) do
    up_or_down = (thresh > 0) ? 'up' : 'down'
    "#{ps_or_mps}/all_#{up_or_down}.#{ps_or_mps}"
  end
  
  ps_mps_task(all_up_down_proc, norm_avg_proc) do |ps_or_mps, f|
    all_thresh_filter = make_all_col_thresh_filter(thresh)
    filt_data = apply_proc(f.prerequisites[0], &all_thresh_filter)
    all_col_sorter = make_all_col_sorter(thresh)
    sort_data = apply_proc(filt_data, &all_col_sorter)
    File.write(f.name, sort_data.join)
  end
  
  expand_id(all_up_down_proc)
end

###################  Save the data into a tokyo_hash
# Data_name
# control_name
# treatments
########## open a tokyo_hash, save the values in hash format
# hash[mps] or hash[ps] = treat_hash[treat] = value (l2 ratio to control)
# and make one that is the same, value not ratio

# so working with repaverage ps and mps
# and workging with normaverage ps and mps

#load the file, and insert each row into the tokyo_hash, name the tokyo_hash

# the tokyo_hashes will get stored as assigned by the newtokyo_hash function
# this is a problem, because my function relies on tracking the file status in order to know when to update the task.  I can just touch a file to signal that the data storage has been completed.

{'rep_avg' => rep_average_proc,
'norm_avg' => norm_avg_proc}.each do |avg_type, data_proc|
  tokyo_hash_proc = ->(ps_or_mps) do
    # strip this with File.basename down below to make the actual
    # tokyo_hash name.  This file will be touched when the tokyo_hash
    # has been created.
    "#{ps_or_mps}/#{Data_name}_#{ps_or_mps}_#{avg_type}.tokyo"
  end
  ps_mps_task(tokyo_hash_proc, data_proc) do |ps_or_mps, f|
    # f.prerequisites[0] will have either rep_average, or norm_average
    # f.name will by the tokyo_hash
    store_to_tokyo_hash(f.prerequisites[0], File.basename(f.name,'.tokyo'))
    sh "touch #{f.name}"
  end
end


=begin

now we need to get down to business,  Find up and down regulated genes

1) Normalize data vs control
2) Label with gs_nm_ps or gs_nm_mps
3) For each condition, two fold up, two fold down
4) Same thing but with fdr, rely on matlab for this
5) 1 bed files for each condition, signal relative to control ps and mps
    This show up/down regulation at the probe or mps level
6) Ratio bedfiles
    ps (full) / mps (comprehensive) helps to see intron vs exon (pre vs mature)
    ps (comprehensive) / mps (comprehensive) helps to look for isoform variation

=end
