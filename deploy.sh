#!/bin/sh
# Compile and push Hakyll-generated site up to the server using scp
## set as a Git hook for more awesomeness

DOMAIN="benjeffrey.com"
HAKYLL_SCRIPT="hakyll.hs"
HAKYLL_EXEC="hakyll"
WEBSERVER="parsley"

# echo "********************************************************************************"
echo "\nBeginning post-commit site upload."

# recompile the binary, just in case
echo "\nRecompiling ${HAKYLL_SCRIPT} (GHC and Hakyll must be installed)..."
ghc --make ${COMPILE_EXECUTABLE}

echo "\nRebuilding site..."
./${HAKYLL_EXEC} rebuild

echo "\nUploading site to ${WEBSERVER}..."
scp -rC ./_site/* ${WEBSERVER}:/var/www/${DOMAIN}/

echo "\nUploading Nginx configuration..."
scp -C ./deployment/site.nginx ${WEBSERVER}:/etc/nginx/sites_enabled/${DOMAIN}
echo "\nSite upload complete! Check it out at http://${DOMAIN}\n"
# echo "********************************************************************************"
