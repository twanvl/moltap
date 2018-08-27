# My formating code

package format_tex;
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
  s/(--.*)/my_unmeddle("<span class=\"comment\">$1<\/span>")/ge;
  s/(?<!~)("([^"\\]|\\.)*?")/my_unmeddle("<span class=\"str\">$1<\/span>")/ge;
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
  $_ = my_tex_to_html($_);
  #$_ = "<span class=\"math\">$_</span>";
  return my_unmeddle($_);
}

sub my_format_pre {
  $_ = @_[0];
  s@<img src="([^"]*)" alt="[^"]*">@\\includegraphics[scale=0.5]{../../website/$1}@g;
  s@<span class="output">(.*?)</span>@\\textit{$1}@g;
  s@<span class="prompt">(.*?)</span>@\\textbf{$1}@g;
  s@[\$\^&]@"\\".$&."{}"@ge;
  #print "[[$_]]\n";
  $_ = my_meddle($_);
  return my_unmeddle($_);
}
sub my_format_term {
  $_ = @_[0];
  s@&(phi|psi);@\\$1@g;
  s@&not;@\\neg$1@g;
  s@[{}&_#]@\\$&@g;
  s@(\\(amp|et|vee|neg|imp|pmi|eq|neq|not\\eq|top|bot|phi|psi|chi|alpha|beta|BB|DD))@\\ensuremath{$1}@g;
  s@(?<!&)(\\\#.*)@\\termcomment{$1}@g;
  s@\b(?<![\\])(let|true|false|and|or|implies|not|system)@\\keyword{$1}@g;
  s@((?:[KM]|\\ensuremath{\\[BD][BD]}|\[\]|\<\>)[*]?)\\_((\w|[{},\\])*)@$1\\agents{$2}@g;
  s@((?:[KM]|\\ensuremath{\\[BD][BD]}|\[\]|\<\>)[*]?)\\_\\_(?!<)(\w*)@$1\\_\\agents{$2}@g;
  return my_unmeddle($_);
}
sub my_format_term_simple {
  $_ = @_[0];
  s@&(phi|psi|alpha|beta);@\\$1@g;
  s@[{}&_]@\\$&@g;
  s@(\\(amp|et|vee|neg|imp|pmi|eq|neq|not\\eq|top|bot|phi|psi|chi|alpha|beta))@\\ensuremath{$1}@g;
  return my_unmeddle($_);
}
sub my_nice_term {
  $_ = @_[0];
  s@&@&and;@g;
  s@\|@&or;@g;
  s@~@&not;@g;
  s@->@&rarr;@g;
  s@<-@&larr;@g;
  return $_;
}

sub my_format_table_row {
  $_ = @_[0];
  s@\^\^\^@@g;
  my @cols = split /(?=^[|!]|  [|!])/;
  my $section = /^[|]|  [|]/ ? "tbody" : "thead";
  my $result = "";
  my $colcode = "|";
  foreach (@cols) {
    s/^\s*[|\!]\s*//;
    my $properties = '';
    my $colspan = 1, my $rowspan = 1;
    if (/^\{([0-9]+)(?:,([0-9]+))?\}\s*(.*)/) {
       $colspan = $1 if $1>1;
       $rowspan = $2 if $2>1;
       $_ = $3;
    }
    for (my $i = 0 ; $i < $colspan ; ++$i) {
      $colcode .= 'l|';
    }
    $_ = "\\multicolumn{$colspan}{|".($section eq 'tbody' ? 'l' : 'c')."|}{$_}" if $colspan > 1;
    $result .= " & " if $result;
    $result .= " $_";
  }
  return "\\begin{tabular}{$colcode}\n\\hline<$section> $result<$section>\\\\\\hline\\end{tabular}";
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
  $url  = my_unmeddle($url);
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
  # Run graphviz?
  my $exists = 0;
  if ($exists and -f "$outdir/$imagedir/$file.png") {
    print "    \t[skip]";
  } else {
    `$type -Tpng $tempdir/$file.dot -o $outdir/$imagedir/$file.png`
  }
  print "\n";
  return "<img src=\"$imagedir/$file.png\" class=\"graph\" alt=\"\">";
}

sub format {
  $_ = @_[0];
  
  s|\r||sg;
  
  # Graphs
  #s|^\s*GRAPH:\s*(?:\[([a-z]+)\])?\s*(\S+)((\n [^\n]*)*)|"</p>".my_format_graph($2,$3,$1)."<p>"|mge;
  #s|^\s*FORMULA:\s*(\S+)((\n [^\n]*)*)|"</p>".my_format_latex($1,$2)."<p>"|mge;
  s|^\s*SCREENSHOT:\s*(\S+)|\\\\\\includegraphics[scale=0.5]{../../website/$1}|mg;
  s|^IFLATEX:\s*(.*)|$1|mge;
  s|^IFHTML:.*||mg;
  
  # no markup
  s|^ (.*)|"</p>".my_unmeddle($1)."<p>"|mge;
  # comment
  s|^%.*||mg;
  s|%%%.*||mg;
  # line continuation
  s|\n\.\.|  |g;
  
  #code block
  s|^> (.*)|"$1</p><pre>".my_format_haskell($1)."</pre><p>"|mge;
  #s|^] (.*)|"\\begin{lstlisting}\n".my_format_pre($1)."\n\\end{lstlisting}"|mge;
  #s|\n\\end{lstlisting}\n\\begin{lstlisting}\n|\n|g;
  s|^] (.*)|"\\begin{code}\\texttt{".my_format_pre($1)."}\\\\\\end{code}"|mge;
  s|\\end{code}\n\\begin{code}|\n|g;
  s|\\\\(\\end{code})|$1|g;
  s|\\begin{code}|\\fbox{\\parbox{15cm}{|g;
  s|\\end{code}|}}|g;
  
  # per line
  s|(?<!~)@([^@]+)@|"\\term{".my_format_term($1)."}"|ge;
  s|'''(.*?)'''|\textbf{$1}|g;
  s|''(.*?)''|\\emph{$1}|g;
  s@<span class="term">(.*?)</span>@"\\term{".my_format_term_simple($1)."}"@ge;
  s@<span class="term agent">(.*?)</span>@"\\term{\\agents{".my_format_term_simple($1)."}}"@ge;
  
  # lists
  s|^\*(.*)|\\begin{itemize}\\item $1\\end{itemize}|mg;
  s|^\#(.*)|\\begin{enumerate}\\item $1\\end{enumerate}|mg;
  s|\\end{itemize}(\n?)\\begin{itemize}|$1|g;
  s|\\end{enumerate}(\n?)\\begin{enumerate}|$1|g;
  
  # tables
  s@^([!|].*)@my_format_table_row($1)@mge;
  s|\\hline\\end{tabular}\n\\begin{tabular}{.*?}\n?|\n|g;
  s|<thead>\\\\\n\\hline<tbody>|\\\\\n\\hline\\hline|g;
  s|<thead>||g;
  s|<tbody>||g;
  s|(\\begin{tabular}{.*?})\n?\\begin{tabular}{.*?}|$1|g; # manually specify size
  
  # markup
  s|^==(.*)==|\\heading{$1}|mg;
  s|^---(.*)---|\\subheading{$1}|mg;
  s|^--(.*)--|\\heading{$1}|mg;
  
  # unescape
  s|~(.)|$1|sg;
  s|~|\\ensuremath{\\sim}|g;
  
  # remove html
  s@&ldquo;@``@g;
  s@&rdquo;@''@g;
  s@<a href="[^"]*">|</a>@@g;
  s@<span class="[^"]*">|</span>@@g;
  s@<kbd>(.*?)</kbd>@"\\kbd{".my_meddle(my_format_pre($1))."}"@ge;
  #s@<br>@\\\\@g;
  s@<br>@@g;
  
  s|MOLTAP|\\MOLTAP{}|g;
  
  # fix stuff
  s|(\heading{.*?}\s*)\\\\|$1|g;
  
  return $_;
}

sub story {
  my($pkg, $path, $filename, $story_ref, $title_ref, $body_ref) = @_;
  $$body_ref = my_format($$body_ref);
  return 1;
}

1;

__END__
