-----
title: Setting-up and using Nginx
description: Some notes for installing/configuring Nginx
date: 2012-10-05
-----


Installation on Ubuntu
----------------------

The following commands let you install Nginx from the stable repo,
rather than the (usually) outdated one in Ubuntu's main repository.

```bash
apt-get install python-software-properties
# For the `add-apt-repository` command
add-apt-repository ppa:nginx/stable
apt-get update
apt-get install nginx
# or alternatively
apt-get updgrade
```


Installation on CentOS 6
------------------------

There is an `nginx-release` RPM package available on the
[Nginx website](http://nginx.org/en/download.html), which will configure the
repos to install Nginx. On CentOS 6:

```bash
wget http://nginx.org/packages/centos/6/noarch/RPMS/nginx-release-centos-6-0.el6.ngx.noarch.rpm
yum install ./nginx-release-centos-6-0.el6.ngx.noarch.rpm
yum install nginx
```


Nginx Configuration
-------------------

The main configuration file is located at `/etc/nginx/nginx.conf`.

whilst the default site configuration is usually to be found in either of:

* `/etc/nginx/conf.d/default.conf`
* `/etc/nginx/sites-available/default`

The default document root on Ubuntu is `/usr/share/nginx/www`.

A few extra options are always handy set:

### Worker Processes

    worker_processes  4;        # take advantage of SMP
    # NOTE: max_clients = worker_processes * worker_connections

### Transactional TCP

    tcp_nopush     on;      # Enables T/TCP, works well with sendfile option

### Gzip compression

    gzip            on          # `off` by default
    gzip_disable    "msie6";    # IE6 cannot handle GZip properly
    gzip_proxied    any;        # Allows GZipping response to proxy request
    gzip_types      text/plain text/html text/css application/x-javascript text/xml application/xml application/xml+rss text/javascript;
                                # By default, only text/html is compressed.

### Virtual Host Configurations

    include /etc/nginx/sites-available/*



Host configuration
----------------------------------------

Host configurations are defined and kept in two locations:

    /etc/nginx/sites-available
    /etc/nginx/sites-enabled

If they don't exist you can just make them yourself. The first holds the actual
configuration files, which you link to from the `sites-enabled` directory.
This allows you to add or remove virtually hosted sites from Nginx at will,
without necessarily touching the configuration files.

###Document Root

Create the directory `/var/www/`

    mkdir -p /var/www/

Uncomment both `listen` lines to make Nginx listen on port 80 IPv4 and IPv6.
`server_name _;` is an invalid directive, making this a default catchall vhost
of course, you can as well specify a hostname here like www.example.com).

Now save the file and restart nginx:

    /etc/init.d/nginx restart

Enable the vhost and reload Nginx:

    cd /etc/nginx/sites-enabled/
    ln -s /etc/nginx/sites-available/www.hostmauritius.com
    /etc/init.d/nginx reload








Sources
----------------------------------------

Based on <http://www.howtoforge.com/installing-nginx-with-php5-and-php-fpm-and-mysql-support-on-ubuntu-11.04> and <http://www.howtoforge.com/how-to-set-up-ssl-vhosts-under-nginx-plus-sni-support-ubuntu-11.04-debian-squeeze>
and <http://articles.slicehost.com/2009/2/20/centos-nginx-configuration>
