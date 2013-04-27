
* generate RSA keypair
* generate [Certificate Signing Request][CSR], containing thepublic key
* uplead CSR to Certificate Authority, and get signed key back
* get Certificate Authority's [intermediate certificate][]
* concatenate intermediate certificates to your CA-signed certificate,
    [as the Nginx docs specify][HttpSsl]
* remove passphrase from the private key, and modify permissions


```bash
openssl genrsa -des3 -out benjeffrey.com.key 4096
Enter pass phrase for benjeffrey.com.key:
Verifying - Enter pass phrase for benjeffrey.com.key:
```

```bash
openssl req -new -key benjeffrey.com_encrypted.key -out benjeffrey.com.csr

Enter pass phrase for benjeffrey.com.key:
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:GB
State or Province Name (full name) [Some-State]:Midlothian
Locality Name (eg, city) []:Edinburgh
Organization Name (eg, company) [Internet Widgits Pty Ltd]:Ben Jeffrey
Organizational Unit Name (eg, section) []:.
Common Name (e.g. server FQDN or YOUR name) []:benjeffrey.com
Email Address []:mail@benjeffrey.net

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []: *none*
An optional company name []:

```


Go through the Gandi SSL certificate-signing (validating your domain in
the process) and download your Gandi-signed certificate (`certificate-
ddddd.crt` -> `benjeffrey.com.crt`) as well as their [intermediate certificate][],
[`GandiStandardSSLCA.pem`][Gandi cert].

For Nginx, you need to append all intermediate certificates to your
server certificate:

```bash
cat benjeffrey.com.crt "\n" GandiStandardSSLCA.pem > benjeffrey.com.crt
```

I got the following error when restarting Nginx, at first:

```bash
nginx: [emerg] SSL_CTX_use_certificate_chain_file("/etc/nginx/ssl/benjeffrey.com.crt") failed (SSL: error:0906D066:PEM routines:PEM_read_bio:bad end line error:140DC009:SSL routines:SSL_CTX_use_certificate_chain_file:PEM lib)
```

but it turned out that I just needed a newline `\n` between the two
certificates, so I added one by hand.



Remove the passphrase from the RSA key
--------------------------------------

```bash
openssl rsa -in benjeffrey.com_encrypted.key -out benjeffrey.com.key
```

Sources
-------

* http://nginx.org/en/docs/http/configuring_https_servers.html
* http://wiki.nginx.org/HttpSslModule
* http://wiki.gandi.net/en/ssl
* http://www.westphahl.net/blog/2012/01/03/setting-up-https-with-nginx-and-startssl/
* http://rtcamp.com/tutorials/wordpress-nginx-thawte-ssl/


<!-- links -->

[CSR]: http://en.wikipedia.org/wiki/Certificate_signing_request "Certificate Signing Request"
[intermediate certificate]: http://en.wikipedia.org/wiki/Intermediate_certificate_authorities
[HttpSsl]: http://wiki.nginx.org/HttpSslModule
[nginx https]: http://nginx.org/en/docs/http/configuring_https_servers.html
[Gandi cert]: http://wiki.gandi.net/en/ssl/intermediate
