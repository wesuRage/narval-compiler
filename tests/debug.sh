entry=$(./getentry.sh);
echo -e "b * $entry\nlayout asm\nrun" > cmd.txt.temp;

gdb main < cmd.txt.temp;
rm cmd.txt.temp;