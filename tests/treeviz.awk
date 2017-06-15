#! /usr/bin/gawk -f
#http://alohakun.blog7.fc2.com/?mode=m&no=355
BEGIN {RS = "~@"; printf "digraph G {\n node [shape = record];";}
/^[0-9]/{
s = sprintf("%s [label = \"{%s | {", $1, $1);
for(i = 2; i < NF - 1; i++)
    s = s sprintf("%s | ", $i);
    s = s sprintf("%s}}\"];\n", $i);
    $0 = s;
    while (/([a-zA-Z]+):@([0-9]+)/){
        format = sprintf("\\1 \\3\n %s:\\1 -> \\2;", $1);
        $0 = gensub(/([a-zA-Z]+):@([0-9]+)(.*)$/, format, "g");
    };
    printf " %s\n", $0;
}
END {print "}"}
