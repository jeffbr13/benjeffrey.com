---
title: Setting up Gandi SSL on Nginx
description: How to get an HTTPS padlock and use it, with Nginx
date: 2013-04-30
---

So I decided to add a nice little [HTTPS][] padlock to
[benjeffrey.com][], having read Lukasa's article "[HTTPS All The
Things][lukasa]". After all, some amount of encryption is always better
than none, as a rule[^CAs]. Even for websites without authentication,
such as this one. Esteemed software blogger Jeff Atwood weighs in on the
issue too, asking "[Should All Web Traffic Be Encrypted?][]"[^ch].

![Requisite meme.](/static/img/encrypt-all-the-things.png)

I reckon (in a totally subjective manner) that there's also a certain
element of credibility conferred by that padlock next to your URL -
similar to the comfort of the familiar dotcom, when compared to the
perceived sketchiness of a [.ru](https://en.wikipedia.org/wiki/.ru) or
the hipster-startup vibe of a [.ly](https://en.wikipedia.org/wiki/.ly)
address.

I recently registered my domain at Gandi.net, and since they provide [a
free SSL certificate for the first year][Gandi SSL], there wasn't any
harm in trying one out.

This page documents the process I went through to get and install an SSL
certificate in order to encrypt web traffic passing between my personal
website and you, my esteemed readers!

My setup is just [a static website][building] hosted on an Nginx
webserver, so it wasn't hard to find documentation to set up SSL (see
the [Resources] section for deets). To reproduce my steps, simply follow
the instructions outlined below, replacing `benjeffrey.com` etc. with
your own domain name... You'll figure it out.


Getting your SSL Certificate
----------------------------

**Requirements**: [OpenSSL](https://www.openssl.org/), and a [Certificate
Authority][CA] to verify your certificate.

### Generate an RSA Keypair

Connections with SSL are negotiated using asymmetric public-key
encryption ([approximately][ssl]). The client uses your site's public key
to encrypt packets to the webserver, which then decrypts them with its
private key, before the connection drops to computationally-cheaper
symmetric encryption.

So our first step is generating this aforementioned public/private
keypair with OpenSSl:

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
verification takes a few hours - I left the process for a day, then came
back to it once everything had propagated.

![You've unlocked Gandi.net's certification badge. Wow, this site must be secure!](https://www.gandi.net/static/static/img/ssl/GANDI_SSL_logo_B_std_en.png)

Once this is done, you can download your newly CA-signed certificate (and
rename it to `benjeffrey.com.crt` in the process).

You'll also need [Gandi's intermediate certificate][Gandi cert],
`GandiStandardSSLCA.pem`.


Setting up Nginx for SSL
------------------------

### Configure Certificates and Keys

Since Nginx doesn't combine chain certificates itself, you need to append
any and all intermediate certificates to your server certificate,
according to the [HttpSsl Module docs][HttpSsl]. I found that
the Gandi-issued certificate is missing a newline at the end,
causing Nginx to throw an error unless you add one yourself,
before concatenating it with the intermediate certificate:

```bash
echo "\n" >> benjeffrey.com.crt
cat benjeffrey.com.crt GandiStandardSSLCA.pem > benjeffrey.com.crt
```

Nginx also needs to be able to access the private key we created before,
to decode packets encypted with the public key in your certificate.
Aditionally, the private key should only be accessible to the user
running the nginx process. So we remove the passphrase from the private
key, and change it's owner and permissions to be more restrictive:

```bash
openssl rsa -in benjeffrey.com_encrypted.key -out benjeffrey.com.key
# root usually runs the nginx process:
chown root:root benjeffrey.com.key
chmod 400 benjeffrey.com.key
```


### Configure your Site

The following server directives tell Nginx to

1. serve `benjeffrey.com` over HTTPS only, and
2. to redirect any clients which try to access `http://benjeffrey.com`
    to the equivalent HTTPS address,
    with a [301 Moved Permanently][301] status code:

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

The actual configuration file used for benjeffrey.com is [available on
GitHub][nginx-conf].


Resources
---------

* <http://nginx.org/en/docs/http/configuring_https_servers.html>
* <http://wiki.nginx.org/HttpSslModule>
* <http://wiki.gandi.net/en/ssl>
* <http://www.westphahl.net/blog/2012/01/03/setting-up-https-with-nginx-and-startssl/>
* <http://rtcamp.com/tutorials/wordpress-nginx-thawte-ssl/>


<!-- footnotes -->

[^CAs]: Although all forms of encryption are not created equal.
    As Lukasa [points out][lukasa]:

    > the whole notion of [certificate authorities][CA] is a pretty sketchy one

    The commonly-accepted wisdom seems to be that CAs present a single
    point of failure in the [X.509 Public Key Infrastructure][x509],
    due to the amount of trust placed in them;
    CA credentials have been stolen before, and used to forge
    certificates, showing the dangers to be very real.
[^ch]: Jeff actually concludes that (emphasis mine):

    > We need to work toward making HTTPS easier, faster,
    > and most of all, the default for *logged in* users.

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
[301]: http://en.wikipedia.org/wiki/HTTP_301
