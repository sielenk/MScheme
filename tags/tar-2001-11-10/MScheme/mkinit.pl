#!/usr/local/bin/perl

sub createConstant
{
    local($file, $name) = @_;

    open(FILE, $file);

    $buffer = "";
    while (<FILE>)
    {
        s/;.*//;
        s/(["\\])/\\\1/g;
        
        $buffer .= $_;
    }

    $buffer =~ tr/\n\t / /s;

    print("    String $name = \"$buffer\";\n");
}

print("package MScheme;\n\npublic interface Init\n{\n");

foreach $file (@ARGV)
{
    $file =~ /(.*).scm/;
    $name = $1;
    
    createConstant($file, $name);
    print("\n");
}

print("}\n");
