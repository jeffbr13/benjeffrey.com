# sync compiled HTML
rsync -aze ssh _site/ parsley:/var/www/benjeffrey.com/
# sync Nginx configuration
rsync -aze ssh src/nginx.conf parsley:/etc/nginx/sites_enabled/benjeffrey.com
