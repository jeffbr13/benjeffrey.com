---
title: Setting up HTTPS on Nginx with Gandi
description: How to get an SSL certificate from Gandi and use it with Nginx
date: 2013-04-30
---

So I decided to add a nice little padlock (i.e. [HTTPS][]) to
[benjeffrey.com][], after reading Lukasa's article "[HTTPS All The Things
(Especially This Thing)][lukasa]". Naturally, some level of encryption is
always better than none at all, even if "the whole notion of [certificate
authorities][CA] is a pretty sketchy one"[^CAs]. Even famed software
blogger Jeff Atwood weighs in on the issue, asking "[Should All Web
Traffic Be Encrypted?][]"[^ch].

I also feel that there's some element of credibility conferred by the
padlock in your URL bar, similar to having a `.com` compared to a `.ru`.

![Requisite meme.](/images/encrypt-all-the-things.png)

This page documents the process I went through to get and install an SSL
certificate in order to encrypt web traffic passing between my personal
website and you, esteemed readers!

I recently registered my domain at Gandi.net, and since they provide [a
free SSL certificate for the first year][Gandi SSL], there wasn't any
harm in trying one out.

My [setup][building] is a static website hosted on an Nginx webserver, so
it wasn't too hard to find documentation for setting up SSL (see the
[Resources] section at the end for deets). To reproduce my steps, simply
follow the instructions outlined below.


Getting your SSL Certificate
----------------------------

**Requirements**: [OpenSSL](https://www.openssl.org/), and a [Certificate
Authority][CA] to verify your certificate.

### Generate an RSA Keypair

Connections with SSL are negotiated ([approximately][ssl]) using
asymmetric encryption. The client uses your site's public key to encrypt
packets to the webserver, which decrypts them with its private key,
before the connection drops to computationally-cheaper symmetric
encryption.

So our first step is generating the aforementioned keypair with OpenSSl:

```bash
openssl genrsa -des3 -out benjeffrey.com_encrypted.key 4096
```

This will prompt you to set a passphrase to secure the private part of your keypair. Do that.


### Generate a Certificate Signing Request

A [CSR] is a file sent to Certificate Authorities containing the
information to be included in your SSL certificate, including the public
key that clients will use to communicate with your webserver.

To generate your <abbr title="Certificate Signing Request">CSR</abbr>,
execute:

```bash
openssl req -new -key benjeffrey.com_encrypted.key -out benjeffrey.com.csr
```

And answer the questions you're given.


### Get Your Certificate Signed by Gandi

In the SSL certificate-signing process (which requires you to prove
owneship of your domain) you'll upload your CSR, `benjeffrey.com.csr`, to
Gandi. If you choose the DNS record method, then don't be surprised if
verification takes a few hours, or even days.

![Gandi.net's certification badge! This site must be secure!](https://www.gandi.net/static/images/ssl/GANDI_SSL_logo_B_std_en.png)

Once this is done, you can download your newly CA-signed certificate (and
rename it to `benjeffrey.com.crt` in the process).

You'll also need [Gandi's intermediate certificate][Gandi cert],
`GandiStandardSSLCA.pem`.


Setting up Nginx for SSL
------------------------

### Configure Certificates and Keys

For Nginx, you need to [append all intermediate certificates to your
server certificate][HttpSsl]. There needs to be a newline between
your certificates, otherwise Nginx will throw an error on loading
the certificate file:

```bash
echo "\n" >> benjeffrey.com.crt
cat benjeffrey.com.crt GandiStandardSSLCA.pem > benjeffrey.com.crt
```

Nginx also needs to be able to access the private key we created before,
so we remove the passphrase from it:

```bash
openssl rsa -in benjeffrey.com_encrypted.key -out benjeffrey.com.key
```

Next, upload `benjeffrey.com.crt` and `benjeffrey.com.key`
to `/etc/nginx/ssl` on your server.


### Configure your Site

The following server directives will tell Nginx to serve your website
over HTTPS only, and give 301 redirect HTTP requests to the HTTPS
address:

```
server {
    server_name         benjeffrey.com;
    root                /var/www/benjeffrey.com;
    listen              443 ssl;
    ssl_certificate     /etc/nginx/ssl/benjeffrey.com.crt;
    ssl_certificate_key /etc/nginx/ssl/benjeffrey.com.key;

    location / {
            index       index.html index.htm index;
            try_files   $uri $uri/ $uri.html =404;
    }
}

# redirect HTTP traffic to HTTPS
server {
    server_name         benjeffrey.com
    listen              80;
    return              301 https://benjeffrey.com$request_uri;
}
```

The actual configuration file benjeffrey.com uses is [available on
GitHub][nginx-conf].


Resources
---------

* <http://nginx.org/en/docs/http/configuring_https_servers.html>
* <http://wiki.nginx.org/HttpSslModule>
* <http://wiki.gandi.net/en/ssl>
* <http://www.westphahl.net/blog/2012/01/03/setting-up-https-with-nginx-and-startssl/>
* <http://rtcamp.com/tutorials/wordpress-nginx-thawte-ssl/>


<!-- footnotes -->

[^CAs]: Certificate authorities present a single point of failure in
    the [X.509 Public Key Infrastructure][x509], due to the amount
    of trust placed in them, as has been seen before when
    root certificates have been stolen then used in exploits.
[^ch]: Jeff actually concludes that (emphasis mine):

    > We need to work toward making HTTPS easier, faster,
    > and most of all, the default for logged in users.

    but, for tiny sites with equally tiny hosting costs, why not extend
    the grace to everyone?



<!-- links -->

[CSR]: http://en.wikipedia.org/wiki/Certificate_signing_request "Certificate Signing Request"
[HttpSsl]: http://wiki.nginx.org/HttpSslModule
[nginx https]: http://nginx.org/en/docs/http/configuring_https_servers.html
[Gandi cert]: http://wiki.gandi.net/en/ssl/intermediate
[lukasa]: https://lukasa.co.uk/2013/03/HTTPS_All_The_Things/
[HTTPS]: http://en.wikipedia.org/wiki/HTTP_Secure
[benjeffrey.com]: https://benjeffrey.com
[Gandi]: https://www.gandi.net/
[Gandi SSL]: https://www.gandi.net/domain/ssl
[building]: https://benjeffrey.com/posts/building-benjeffrey.com-with-hakyll
[verify]: https://www.gandi.net/ssl/secured/benjeffrey.com/30951/ccb4de3b85
[Should All Web Traffic Be Encrypted?]: http://www.codinghorror.com/blog/2012/02/should-all-web-traffic-be-encrypted.html
[CA]: http://en.wikipedia.org/wiki/Certificate_authority
[x509]: http://en.wikipedia.org/wiki/X.509
[ssl]: http://en.wikipedia.org/wiki/Secure_Sockets_Layer#Simple_TLS_handshake
[nginx-conf]: https://github.com/jeffbr13/benjeffrey.com/blob/master/nginx
