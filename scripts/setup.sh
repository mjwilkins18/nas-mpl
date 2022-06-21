curr_dir=$(pwd)
path='$PATH'
alias_str='"mpl -default-type int64 -default-type word64"'

if [ ! -d "tool_src/mlton" ]; then
	mkdir tool_src/mlton
	tar -xf tool_src/mlton-20210117-1.amd64-linux-glibc2.31.tgz -C tool_src/mlton --strip-components=1
fi

export PATH=$curr_dir/tool_src/mlton:$PATH
export PATH=$curr_dir/tool_src/mlton/bin:$PATH
cd "tool_src/mpl"
make all
cd $curr_dir

echo "export PATH=$curr_dir/tool_src/mpl/build/bin:$path
export PATH=$curr_dir/mlton:$path
export PATH=$curr_dir/mlton/bin:$path
alias MPL=$alias_str" > ENV
