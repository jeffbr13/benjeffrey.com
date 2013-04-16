# Compile and push Hakyll-generated site up to the server using scp
## set as a Git hook for more awesomeness

echo "********************************************************************************"
echo "Beginning post-commit site upload."
## recompile the binary, just in case
echo "\nRecompiling site.hs (GHC and Hakyll must be installed)..."
ghc --make hakyll.hs
echo "\nRebuilding site..."
./site rebuild
echo "\nUploading site..."
scp -rC ./_site/* parsley:/var/www/benjeffrey.com/
echo "\nUploading Nginx configuration..."
scp -C ./deployment/site.nginx parsley:/etc/nginx/sites_enabled/benjeffrey.com
echo "\nSite upload complete! Check it out at http://benjeffrey.com"
echo "********************************************************************************"
