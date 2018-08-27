# My formating code

package format;
use strict;
use Cwd;

# --- Configurable variables -----

our $tempdir  = "dist/build/website";
our $outdir   = "dist/website";
our $imagedir = "image";

# --------------------------------

sub write_if_different {
  my ($filename, $body) = @_;
  if (-f $filename) {
    open IN, "<$filename";
    my @old = <IN>;
    close IN;
    return 1 if (join('',@old) eq $body);
  }
  open OUT, ">$filename";
  print OUT $body;
  close OUT;
  return 0;
}

# --------------------------------

sub start {
  return 1;
}

sub my_meddle {
  $_ = @_[0];
  s|~(.)|$1|sg; # allow further medelling
  return $_;
}
sub my_unmeddle {
  $_ = @_[0];
  s|(.)|~$1|sg; # prevent further medelling
  return $_;
}
sub my_no_escape {
  $_ = @_[0];
  s|&lt;|<|g;
  s|&amp;|&|g;
  return $_;
}
sub my_format_haskell {
  my $small;
  ($_,$small) = @_;
  $small &&= length($_) < 15;
  my $prefix = '';
  if (!$small && /^(\*[a-z]+>|<[a-z]+> *>?|> )(.*)/i) {
    $prefix = $1;
    $_ = $2;
    $prefix =~ s|&|&amp;|g;
    $prefix =~ s|<|&lt;|g;
    $prefix = "<span class=\"input\">$prefix</span>";
  }
  s|&|&amp;|g;
  s@&amp;([a-z]+|#[0-9]+);@&$1;@g;
  s|<|&lt;|g;
  #s|~|~~|sg; # escape
  s|!!!(.*?)!!!|my_unmeddle(my_no_escape($1))|ge;
  #s/(--.*)/my_unmeddle("<span class=\"comment\">$1<\/span>")/ge;
  s/(--.*)/my_unmeddle("<span class=\"comment\">".my_no_escape($1)."<\/span>")/ge;
  #s/(?<!~)("([^"\\]|\\.)*?")/my_unmeddle("<span class=\"str\">$1<\/span>")/ge;
  s/(?<!~)("([^"\\]|\\.)*?")/my_unmeddle("<span class=\"str\">".my_no_escape($1)."<\/span>")/ge;
  s/(?<=[ (\[.])('([^'\\]|\\.)*?')/my_unmeddle("<span class=\"chr\">$1<\/span>")/ge;
  s/\b(if|then|else|module|import|where|let|in|case|of|newtype|(data|type|default|infix|infixr|infixl)( family)?|class|instance|forall|exists)\b/my_unmeddle("<span class=\"keyword\">$1<\/span>")/ge;
  s/({-#.*#-})/my_unmeddle("<span class=\"pragma\">$1<\/span>")/ge;
  s/(\s|[-+!@*\/.\[(,])([0-9]+)/$1.my_unmeddle("<span class=\"num\">$2<\/span>")/ge;
  s/\b([A-Z]([A-Za-z0-9_]|[.][A-Z])+[.]?)/my_unmeddle("<span class=\"conid\">$1<\/span>")/ge;
  s/(?<=[^!@\#\$%^*\-.|\\=+>;~])(=>|=|->|::|\\|&lt;-|\|)(?=\s|[~a-zA-Z(@])/my_unmeddle("<span class=\"keyglyph\">$1<\/span>")/ge;
  s/(\[|\])/my_unmeddle("<span class=\"listcon\">$1<\/span>")/ge;
  s/(?<!~)(:([-!@\#\$%^*\.|\\=+>]|&amp;|&lt;|~~)*|`[A-Z][a-zA-Z0-9_]+`)/my_unmeddle("<span class=\"conop\">".my_meddle($1)."<\/span>")/ge;
  s/(?<!~)(([-!@\#\$%^*\.|\\=+>]|&amp;|&lt;|~~)+|`[a-zA-Z0-9_]+`)/my_unmeddle("<span class=\"varop\">".my_meddle($1)."<\/span>")/ge;
  $_ = my_meddle($_);
  if ($small) {
    s/ /&#160;/g;
    s/<span&#160;/<span /g;
  }
  return $prefix . my_unmeddle($_);
}
sub my_format_math {
  $_ = @_[0];
  s|&|&amp;|g;
  s|<|&lt;|g;
  $_ = my_tex_to_html($_, 1);
  #$_ = "<span class=\"math\">$_</span>";
  return my_unmeddle($_);
}

sub my_format_pre {
  $_ = @_[0];
  #s|!!!(.*?)!!!|my_unmeddle(my_no_escape($1))|ge;
  #s|(?<!~)&|&amp;|g;
  #s@&amp;([a-z]+|#[0-9]+);@&$1;@g;
  #s|(?<!~)<|&lt;|g;
  $_ = my_meddle($_);
  return my_unmeddle($_);
}
sub my_format_term {
  $_ = @_[0];
  s|&|&amp;|g;
  s|~~|~|sg; # unescape
  s@&amp;([a-z]+|#x?[0-9]+);@&$1;@g;
  s@<@&lt;@g;
  $_ = my_tex_to_html($_, 0);
  #print "$_\n";
  s@<span class="[^"]+">([^<]+)</span>@$1@g;
  s@(\||&(amp|and|nbsp|or|not|rarr|larr|harr|ne|\#\d{4});|~|->|&lt;-/->|&lt;->|&lt;-|/=|=|(?<![a-z0-9]);)@<span class="sym">$1</span>@g;
  s/(?<!&)(\#.*)/<span class="comment">$1<\/span>/g;
  s@\b(?<!&)(let|true|false|and|or|implies|not|system)@<span class="kw">$1</span>@g;
  s@\b([KM])\*\b@<span class="ksym">$1<sup>*</sup></span>@g;
  s@\b([KM])\b@<span class="ksym">$1</span>@g;
  s@([KM])_(?!_)((\w|[{},])*)\*@<span class="ksym">$1<sub>$2</sub><sup>*</sup></span>@g;
  s@([KM])_(?!_)((\w|[{},])*)@<span class="ksym">$1<sub>$2</sub></span>@g;
  s@(#9633;</span>|#9671;</span>|\[\]|&lt;\>)_(?!_)((\w|[{},])*)@$1<sub>$2</sub>@g;
  s@([KM])__(?!<)(\w*)@<span class="ksym">$1_<span class="agent">$2</span></span>@g;
  s@__@_@g;
  s@\[((\w|[{},])*)\]@<span class="ksym">[<span class="agent">$1</span>]</span>@g;
  s@&lt;((\w|[{},])*)>@<span class="ksym">&lt;<span class="agent">$1</span>></span>@g;
  s@[(){}\[\]]+@<span class="par">$&</span>@g;
  #print "$_\n";
  #$_ = "<span class=\"term\">$_</span>";
  return my_unmeddle($_);
}
sub my_nice_term {
  $_ = @_[0];
  s|~~|~|sg; # unescape
  s@&@&and;@g;
  s@\|@&or;@g;
  s@~@&not;@g;
  s@->@&rarr;@g;
  s@<-@&larr;@g;
  return $_;
}
sub my_tex_to_html {
  $_ = @_[0];
  my $subscript = @_[1];
  #print "$_\n";
  s@\\Pos@^+@g;
  s@\\Neg@^-@g;
  if ($subscript) {
    s@_{(.*?)}@<sub>$1</sub>@g;
    s@_([^\\]|\\[a-z]+)@<sub>$1</sub>@gi;
    s@\^{(.*?)}@<sup>$1</sup>@g;
    s@\^([^\\]|\\[a-z]+)@<sup>$1</sup>@gi;
  }
  s@\\New[{](.*?)[}]@<span class="new">$1</span>@g;
  s@\\Old[{](.*?)[}]@<span class="old">$1</span>@g;
  s@\\T\b@<span class="box">&#8866;</span>@g;
  s@\\not\\models\b@<span class="box">&#8877;</span>@g;
  s@\\models\b@<span class="box">&#8872;</span>@g;
  s@\\et\b@<span class="sym">&and;</span>@g;
  s@\\vee\b@<span class="sym">&or;</span>@g;
  s@\\top@<span class="sym">&#8868;</span>@g;
  s@\\bot@<span class="sym">&#8869;</span>@g;
  s@\\BB@<span class="box">&#9633;</span>@g;
  s@\\DD@<span class="box">&#9671;</span>@g;
  s@\\emptyset@<span class="sym">&empty;</span>@;
  s@\\imp@<span class="sym">&rarr;</span>@g;
  s@\\pmi@<span class="sym">&larr;</span>@g;
  s@\\not\\eq@<span class="sym">&#8622;</span>@g;
  s@\\eq@<span class="sym">&harr;</span>@g;
  s@\\neq@<span class="sym">&ne;</span>@g;
  s@\\Imp@<span class="sym">&rArr;</span>@g;
  s@\\Eq@&<span class="sym">hArr;</span>@g;
  s@\\neg@<span class="sym">&not;</span>@g;
  s@\\g\b@<span class="greek">&gamma;</span>@g;
  s@\\d\b@<span class="greek">&delta;</span>@g;
  s@\\phi@<span class="greek">&phi;</span>@g;
  s@\\psi@<span class="greek">&psi;</span>@g;
  s@\\chi@<span class="greek">&chi;</span>@g;
  s@\\G(amma)?\b@<span class="greek">&Gamma;</span>@g;
  s@\\D(elta)?\b@<span class="greek">&Delta;</span>@g;
  s@\\E(psilon)?\b@<span class="greek">&Epsilon;</span>@g;
  s@\\Phi@<span class="greek">&Phi;</span>@g;
  s@\\Psi@<span class="greek">&Psi;</span>@g;
  s@\\cup@<span class="sym">&cup;</span>@g;
  s@\\cap@<span class="sym">&cap;</span>@g;
  s@\\in@<span class="sym">&isin;</span>@g;
  s@\\notin@<span class="sym">&notin;</span>@g;
  s@\\forall@<span class="sym">&forall;</span>@g;
  s@\\exists@<span class="sym">&exist;</span>@g;
  s@\\pm@<span class="sym">&plusmn;</span>@g;
  #s@\\not<span class="(box|sym)">([^<]+)</span>@<span class="$1">$2&#824;</span>@g;
  #s@\\not<span class="(box|sym)">([^<]+)</span>@<span class="$1"><span class="strike">/</span>$2</span>@g;
  s@\\world{(.*?)}@[$1]@g;
  s@\\n{(.*?)}@<span class="sf">$1</span>@g;
  s@\\tup{(.*?)}@&lang;$1&rang;@g;
  s@\\(True|False)\b@<span class="kw">$1</span>@g;
  s@[\[\]\(\)\{\}]|&[lr]ang;@<span class="par">$&</span>@g;
  s@\\;\s*@&nbsp;@g;
  s@\\[a-zA-Z]*@die("Unrecognized latex code: '$&'")@ge;
  #print "   -->  $_\n";
  return $_;
}

sub my_format_table_row {
  $_ = @_[0];
  s@[|!]\^\^\^@@g;
  my @cols = split /(?=^[|!]|  [|!])/;
  my $section = /^[|]|  [|]/ ? "tbody" : "thead";
  my $result = "</p><table><$section><tr>";
  foreach (@cols) {
    my $td = (/^\s*!/) ? "th" : "td";
    s/^\s*[|\!]\s*//;
    my $properties = '';
    if (/^\{([0-9]+)(?:,([0-9]+))?\}\s*(.*)/) {
       $properties .= " colspan=\"$1\"" if $1>1;
       $properties .= " rowspan=\"$2\"" if $2>1;
       $_ = $3;
    }
    $result .= "<$td$properties>$_</$td>";
  }
  $result .= "</tr></$section></table><p>";
  return $result;
}
sub my_format_link {
  $_ = @_[0];
  my $url, my $text, my $class = '';
  if (/^EX: (.*)/) {
    $url   = "prover.html?term=" . $1;
    $text  = my_format_term(my_nice_term($1));
    $class = ' class="term"';
  } elsif (/^CONT: (.*?)\|(.*)/) {
    $url   = $1;
    $text  = "Continue reading: $2";
    $class = ' class="continue"';
  } elsif (/^(.*?)|(.*)/) {
    $url   = $1;
    $text  = $2;
  } else {
    $text  = $url = $_;
  }
  #$url  = my_unmeddle($url);
  return "<a href=\"$url\" $class>$text</a>";
}
sub my_anchor_for {
  $_ = @_[0];
  s/^\s*|\s*$//g;
  s/\s+/-/g;
  return lc $_;
}

# Run a graph through neato
sub my_format_graph {
  my ($file,$body,$type) = @_;
  $type = 'fdp' if not $type;
  print "    graph: $file";
  # preprocess
  $body =~ s/\\Pos/^+/g;
  $body =~ s/\\Neg/^-/g;
  $body =~ s/(&and;|\\et)/∧/g;
  $body =~ s/(&or;|\\vee)/∨/g;
  $body =~ s/(&box;|\\BB)/□/g;
  $body =~ s/(&dia;|\\DD)/◇/g;
  $body =~ s/(&rarr;|\\imp)/→/g;
  $body =~ s/(&larr;|\\pmi)/←/g;
  $body =~ s/(&harr;|\\eq)/↔/g;
  $body =~ s/(&not;|\\neg)/¬/g;
  $body =~ s/(&Gamma;|\\G)/Γ/g;
  $body =~ s/(&Delta;|\\D)/Δ/g;
  $body =~ s/(&phi;|\\phi)/φ/g;
  $body =~ s/(&psi;|\\psi)/ψ/g;
  $body =~ s/(&chi;|\\chi)/χ/g;
  $body =~ s@\^\+@<font face="Arial Unicode MS">⁺</font>@g;
  $body =~ s@\^\-@<font face="Arial Unicode MS">⁻</font>@g;
  $body =~ s@_1@<font face="Arial Unicode MS">₁</font>@g;
  $body =~ s@_2@<font face="Arial Unicode MS">₂</font>@g;
  # Write graphviz input file
  my $exists = write_if_different("$tempdir/$file.dot",
      "digraph G{\n".
      "  graph[dpi=84];\n".
      #"  node[shape=record,fontname=\"Bitstream Cyberbit\",fontsize=15];\n".
      "  node[shape=circle,fontname=\"Bitstream Cyberbit\",fontsize=15];\n".
      "  $body;\n".
      "};\n");
  # Run graphviz?
  if ($exists and -f "$outdir/$imagedir/$file.png") {
    print "    \t[skip]";
  } else {
    `$type -Tpng $tempdir/$file.dot -o $outdir/$imagedir/$file.png`
  }
  print "\n";
  return "<img src=\"$imagedir/$file.png\" class=\"graph\" alt=\"\">";
}
# Run a formula through latex
sub my_format_latex {
  my ($file,$body) = @_;
  print "    latex: $file";
  # Write latex file
  my $exists = write_if_different("$tempdir/$file.tex",
      "\\documentclass[12pt]{article}\n".
      "\\pagestyle{empty}\n".
      "\\input{prelude}\n".
      "\\begin{document}\n".
      "\\begin{align*}$body%\n\\end{align*}\n".
      "\\end{document}\n");
  # Run latex?
  if ($exists and -f "$outdir/$imagedir/$file.png") {
    print "    \t[skip]";
  } else {
    unlink "$outdir/$imagedir/$file.png";
    my $cwd = cwd(); chdir($tempdir);
    `latex $file.tex`;
    chdir($cwd);
    `dvipng $tempdir/$file.dvi -gamma 1.2 -D 120 -T tight --strict -o $outdir/$imagedir/$file.png -Q 12`;
  }
  print "\n";
  return "<img src=\"$imagedir/$file.png\" class=\"latex\" alt=\"\">";
}
sub drop_indent {
  $_ = @_[0];
  s/^  //mg;
  return $_;
}


sub my_format {
  $_ = @_[0];
  
  s|\r||sg;
  s|~|~~|sg; # escape
  
  # Graphs
  s|^\s*GRAPH:\s*(?:\[([a-z]+)\])?\s*(\S+)((\n [^\n]*)*)|"</p>".my_format_graph($2,$3,$1)."<p>"|mge;
  s|^\s*FORMULA:\s*(\S+)((\n [^\n]*)*)|"</p>".my_format_latex($1,$2)."<p>"|mge;
  s|^\s*SCREENSHOT:\s*(\S+)|"</p><img src=\"$1\" class=\"screenshot\"><p>"|mge;
  s|^\s*ALGORITHM:\s*([^\n]*)((\n [^\n]*)*)|"</p><div class=\"algo\"><div class=\"name\"><b>Algorithm</b> $1</div>".my_format(drop_indent($2))."</div><p>"|mge;
  s|^IFLATEX:.*||mg;
  s|^IFHTML:\s*(.*)|$1|mge;
  
  # no markup
  s|^ (.*)|"</p>".my_unmeddle($1)."<p>"|mge;
  # comment
  s|^%.*||mg;
  s|%%%.*||mg;
  # line continuation
  s|\n\.\.|  |g;
  
  #code block
  s|^(\. ?)?> (.*)|"$1</p><pre>".my_format_haskell($2)."</pre><p>"|mge;
  s|^(\. ?)?] (.*)|"$1</p><pre>".my_format_pre(    $2)."</pre><p>"|mge;
  s|</pre><p>(\n)</p><pre>|~$1|g;
  
  # per line
  s|(?<!~)@@([^@]+)@@|"<tt>".my_format_haskell($1,1)."</tt>"|ge;
  s|(?<!~)@([^@]+)@|"<span class=\"term\">".my_format_term($1)."</span>"|ge;
  s|(?<!~)\$([^\$]+)\$|"<span class=\"math\">".my_format_math($1)."</span>"|ge;
  s|'''(.*?)'''|<strong>$1</strong>|g;
  s|''(.*?)''|<em>$1</em>|g;
  s|\[\[(.*?)\]\]|my_format_link($1)|ge;
  
  # lists
  s|^\*(.*)|</p><ul><li>$1</li></ul><p>|mg;
  s|^\#(.*)|</p><ol><li>$1</li></ol><p>|mg;
  s|</ul><p>(\n?)</p><ul>|$1|g;
  s|</ol><p>(\n?)</p><ol>|$1|g;
  while (s|(</li></[uo]l><p>)\n\.(.*)?|\n<cont>$2<cont>$1|mg) {}
  s|<cont> *</p>|<cont>|g;
  s|<p><cont>|<cont>|g;
  s|<cont>||g;
  
  # deflists
  s|^\;(.*)|</p><dl><dt>$1</dt></dl><p>|mg;
  s|^\:(.*)|</p><dl><dd>$1</dd></dl><p>|mg;
  s|</dl><p>\n\n</p><dl>|\n|g;
  s|</dl><p>(\n?)</p><dl>|$1|g;
  s|</dd>(\n?)<dd>|$1|g;
  
  # tables
  s@^([!|].*)@my_format_table_row($1)@mge;
  s|</table><p>\n</p><table>|\n|g;
  s|</thead>\n<thead>|\n|g;
  s|</tbody>\n<tbody>|\n|g;
  
  # markup
  s|^==(.*)==|</p><h2>$1</h2><p>|mg;
  s|^---(.*)---|</p><h3>$1</h3><p>|mg;
  s|^--(.*)--|"</p><h2><a name=\"".my_anchor_for($1)."\"></a>$1</h2><p>"|mge;
  s|\n\n|</p>\n<p>|g;
  $_ = "<p>$_</p>";
  s|<p>(\n?)</p>|$1|g;
  
  # unescape
  s|~(.)|$1|sg;
  
  s|MOLTAP|<abbr class="moltap">$&</abbr>|g;
  
  return $_;
}

sub story {
  my($pkg, $path, $filename, $story_ref, $title_ref, $body_ref) = @_;
  $$body_ref = my_format($$body_ref);
  return 1;
}

1;

__END__
