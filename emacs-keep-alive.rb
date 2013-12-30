#! /usr/bin/env ruby

def get_emacs_pid
  `/usr/local/bin/emacsclient -e '(emacs-pid)' 2>/dev/null`.chomp
end

emacs_pid = get_emacs_pid
if emacs_pid.empty?
  system('/Applications/Emacs.app/Contents/MacOS/Emacs --daemon 2>/dev/null')
  emacs_pid = get_emacs_pid
end

#puts "Tracking " + emacs_pid

while not get_emacs_pid.empty? do
  sleep 10
end
