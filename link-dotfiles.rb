#! /usr/bin/env ruby
require 'fileutils'

prelude_dir = File.expand_path("~/prelude")

dotfiles_map = {
  "bash_profile.bash" => "~/.bash_profile",
  "bashrc.bash" => "~/.bashrc",
  prelude_dir => "~/.emacs.d",
  "personal.el" => File.join(prelude_dir, "personal/personal.el"),
  "prelude-modules.el" => File.join(prelude_dir, "prelude-modules.el"),
  "bin" => "~/bin",
  "rake" => "~/.rake",
  "tmux.conf" => "~/.tmux.conf",
  "gitconfig" => "~/.gitconfig",
  "ssh_config" => "~/.ssh/config"
}

backup_dir = File.expand_path("~/backup_dotfiles")
dirs_to_make = [backup_dir,
                File.expand_path("~/.ssh")]

dirs_to_make.each do |d|
  Dir.mkdir(d) unless File.exists?(d)
end

unless File.exists? prelude_dir
  `git clone https://github.com/bbatsov/prelude.git #{prelude_dir}`
end

def make_a_link(orig, link, backup_dir)
  abs_orig = File.expand_path(orig)
  abs_link = File.expand_path(link)
  if File.symlink?(abs_link)
    puts "Removing old symlink #{abs_link}"
    FileUtils.rm(abs_link)
  end
  if File.exists?(abs_link)
    puts "Moving old file/dir #{abs_link} to backup"
    FileUtils.mv(abs_link, File.join(backup_dir, File.basename(link)))
  end
  puts "Linking " + abs_orig + " to " + abs_link
  File.symlink(abs_orig, abs_link)
end

dotfiles_map.each do |orig, link|
  make_a_link(orig, link, backup_dir)
end

user_launch_agents =  "~/Library/LaunchAgents/"

darwin_links = {
  "org.gnu.emacs.plist" => user_launch_agents + "org.gnu.emacs.plist"
  #  , "fix-podcast.plist" => user_launch_agents + "fix-podcast.plist"
}

if `uname`.chomp == 'Darwin'
  darwin_links.each do |orig, link|
    make_a_link(orig, link, backup_dir)
  end
end
