#!/usr/bin/perl
#
# This script translates *.scm files into String members of an interface.
#  Copyright (C) 2001  Marvin H. Sielenkemper
#
# This file is part of MScheme.
#
# MScheme is free software; you can redistribute it and/or modify 
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation; either version 2 of the License, 
# or (at your option) any later version. 
#
# MScheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details. 
#
# You should have received a copy of the GNU General Public License
# along with MScheme; see the file COPYING. If not, write to 
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.


sub createConstant
{
    local($file, $name) = @_;

    open(FILE, $file);

    $buffer = "";
    while (<FILE>)
    {
        s/;.*//;          # remove comments
        s/(["\\])/\\\1/g; # protect specials
        
        # add a space to avoid errors
        $buffer .= $_ . ' ';
    }

    $buffer =~ s/\s+/ /g;                # squeeze whitespace
    $buffer =~ s/ ?(\(|\)|\[|\]) ?/\1/g; # squeeze even more

    print("    String $name = \"$buffer\";\n");
}

print("package mscheme;\n\npublic interface Init\n{\n");

foreach $file (@ARGV)
{
    $file =~ /(.*).scm/;
    $name = $1;
    
    createConstant($file, $name);
    print("\n");
}

print("}\n");
