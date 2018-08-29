
echo =======================================================================
echo MOLTAP - build and install
echo =======================================================================

# -----------------------------------------------------------------------
# Parse options

# Are we on windows?
if [ -d C:\\ ]; then
  exe='.exe';
else
  exe='';
fi

# Install into webserver path
installdir='/i/i/fmf/moltap';
# Options for doc building
doc_opts=;
build_prog=true;
build_doc=true;

for option in $*; do
  case $option in
    --help)
       echo Usage: $0 [OPTIONS]
       echo
       echo '   --help     Show this help page'
       echo '   --user     Include locally installed libraries in the build process'
       echo '   --full     Full rebuild of documentation'
       echo "   --no-doc   Don't build the documentation"
       echo "   --no-prog  Don't build the program"
       echo
       exit;;
    --user)
       configure_opts='--user';;
    --full)
       doc_opts='--full';;
    --no-doc)
       build_doc=;;
    --no-prog)
       build_prog=;;
    *)
       echo Option $option not supported, see $0 --help for usage information
       echo
       exit;;
  esac
done


# -----------------------------------------------------------------------
echo
if [ $build_prog ]; then
  echo Building Haskell program...

  cabal configure && cabal build || exit;
fi

# -----------------------------------------------------------------------
echo
echo Setting up website/copying...

mkdir -p dist/build/website
mkdir -p dist/website/style
mkdir -p dist/website/image
mkdir -p dist/website/model
cp -u dist/build/moltap-cgi/moltap-cgi$exe dist/website/moltap.cgi
cp -u tools/prelude.tex   dist/build/website
cp -u tools/proof.sty     dist/build/website
cp -u tools/morespace.sty dist/build/website

# -----------------------------------------------------------------------
echo
if [ $build_doc ]; then
  echo Building documentation...

  perl -Itools tools/convert.pl $doc_opts
  
  cp -u doc-src/users-guide.tex dist/build/website
  cd dist/build/website
  pdflatex users-guide > /dev/null
  cd ../../..
  cp dist/build/website/users-guide.pdf dist/website/download/moltap-users-guide.pdf
  
  echo
fi

# -----------------------------------------------------------------------
if [ $installdir -a -d $installdir ]; then
  echo Installing...
  
  cp -r dist/website/* $installdir
fi

echo Done.
echo
