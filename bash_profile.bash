# The following three lines are for compiling the kent source tree
#export MYSQLLIBS="/usr/local/mysql/lib/libmysqlclient.a -lz"
#export MYSQLINC=/usr/local/mysql/include
#export MACHTYPE=x86_64
[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

backed_up_dir=~/Google-Drive
export CLICOLOR=true
export R_HOME=/usr/local/opt/r/R.framework/Resources
export FIREDIR=$backed_up_dir/src/FIRE-1.1a
rubyScriptDir=$backed_up_dir/rubyScripts
export RUBYLIB=$rubyScriptDir:$rubyScriptDir/lib:/Volumes/Spin/BigFiles/tc-subseq
export PATH=$PATH:$RUBYLIB:~/bin:$backed_up_dir/bin/IGVTools
export PATH=$PATH:$backed_up_dir/apt-1.15.0-x86_64-apple-lion/bin
PATH=/usr/texbin:$PATH
Spin=/Volumes/Spin
Affy=$Spin/BigFiles/Affy
export AFFX_ANALYSIS_FILES_PATH=$Affy/HG-U133A_2:$Affy/HuEx-1_0/library:$Affy/HuEx-1_0/mps_ps:$Affy/HuGene-1_0
# MatlabRoot=/Applications/MATLAB_R2011a.app
# export DYLD_FALLBACK_LIBRARY_PATH=$DYLD_FALLBACK_LIBRARY_PATH:$MatlabRoot/bin/maci64/:$MatlabRoot/sys/os/maci64
export JAVA_TOOL_OPTIONS="-Xms250M -Xmx1G"
export BOWTIE2_INDEXES=$backed_up_dir/bowtie2_indexes
alias edit="open -a textedit"
alias xcode="open -a Xcode"
alias excel="open -a Microsoft\ Excel"
alias ai="open -a Adobe\ Illustrator"
alias safari="open -a Safari"
#alias xemacs="open -a Emacs"
alias xmacs="emacsclient -c -n -a ''"
alias emacs="TERM=xterm-256color emacsclient -t -a ''"
alias kill-emacs="emacsclient -t -e '(server-shutdown)'"
alias emacs-pid="emacsclient -e '(emacs-pid)'"
alias run-igv="reattach-to-user-namespace igv"
function file_to_clip_function {
    greadlink -f $1 | perl -ne 'chomp and print' | pbcopy
}
alias file2clip=file_to_clip_function
if hash brew 2>/dev/null
then
    export PATH=/usr/local/bin:/usr/local/sbin:$PATH # brew wants first bat
    export HOMEBREW_EDITOR="emacsclient -c -n -a ''"
    source `brew --prefix`/Library/Contributions/brew_bash_completion.sh
    LESSOPEN="|/usr/local/bin/lesspipe.sh %s"; export LESSOPEN
fi
function parse_git_branch_and_add_brackets {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\ \[\1\]/'
}
PS1="\w\[\033[0;32m\]\$(parse_git_branch_and_add_brackets)\[\033[0m\]\$ "

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

[[ -s "$HOME/.local_bash_profile" ]] && source "$HOME/.local_bash_profile"
