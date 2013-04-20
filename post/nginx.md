-----
title: Basic Nginx configuration
description: A few notes on how to configure Nginx, out of the box
date: 2012-10-05
-----


Installation
------------

You'll usually want to install Nginx from their stable repo, rather than
the older (but more tested) versions in your distro's software
repositories.

### Ubuntu

Note that you may need to install the `python-software-properties`
package in order to use the `add-apt-repository` command on your system.

```bash
add-apt-repository ppa:nginx/stable
apt-get update
apt-get install nginx
```

### CentOS 6

There is an `nginx-release` RPM package available on the [Nginx
website](http://nginx.org/en/download.html), which will configure `yum`
with the repositories needed to install Nginx. For CentOS 6 in
particular, you can run the following commands:

```bash
wget http://nginx.org/packages/centos/6/noarch/RPMS/nginx-release-centos-6-0.el6.ngx.noarch.rpm
yum install ./nginx-release-centos-6-0.el6.ngx.noarch.rpm
yum install nginx
```


Configuration
-------------

The main configuration file is located at `/etc/nginx/nginx.conf`,
while the default site configuration is usually to be found in either of:

* `/etc/nginx/conf.d/default.conf`
* `/etc/nginx/sites-available/default`

The default document root on Ubuntu is `/usr/share/nginx/www`.

A few extra options are always handy set:

### Worker Processes

Helps to ensure that the server doesn't get bogged down by a single
client, as the server calculates `max_clients` as `worker_processes` ✕
`worker_connections` Let's use those cores to their full advantage!

    worker_processes  4;


### Turn on [Gzip][] compression

```conf
gzip            on;
# IE6 can't handle Gzip[!]
gzip_disable    "msie6";
# Allow gzipping responses to proxy requests
gzip_proxied    any;
# Tell Nginx to gzip the following filetypes
gzip_types      text/plain text/html text/css application/x-javascript text/xml application/xml application/xml+rss text/javascript;
```


Virtual Servers
---------------

[When Nginx processes a request](http://nginx.org/en/docs/http/request_processing.html),
it checks which virtual server should process it.

Server configurations are defined and kept in two locations:

    /etc/nginx/sites-available
    /etc/nginx/sites-enabled

The first holds the actual configuration files, which you link to from
the `sites-enabled` directory. This layout allows adding or removing
virtually hosted sites from Nginx at will, without even touching the
configuration files.

### Document Root

You'll need to creat `/var/www/`:

```bash
mkdir -p /var/www/
```

Uncomment both `listen` lines to make Nginx listen on port 80 IPv4 and IPv6.
`server_name _;` is an invalid directive, making this a default catchall vhost
of course, you can as well specify a hostname here like www.example.com).

Now save the file and restart nginx:

```bash
/etc/init.d/nginx restart
```

Enable the vhost and reload Nginx:

```bash
cd /etc/nginx/sites-enabled/
ln -s /etc/nginx/sites-available/www.hostmauritius.com
/etc/init.d/nginx reload
```


Sources
----------------------------------------

* <http://www.howtoforge.com/installing-nginx-with-php5-and-php-fpm-and-mysql-support-on-ubuntu-11.04>
* <http://www.howtoforge.com/how-to-set-up-ssl-vhosts-under-nginx-plus-sni-support-ubuntu-11.04-debian-squeeze>
* <http://articles.slicehost.com/2009/2/20/centos-nginx-configuration>



<!-- links -->

[Gzip]: https://en.wikipedia.org/wiki/Gzip
