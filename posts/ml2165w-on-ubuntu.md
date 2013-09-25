-----
title: The ML-2165W on Ubuntu Raring
description: Setting up the Samsung ML-2165W wireless monochrome laser printer on Ubuntu 13.04
date: 2013-05-08
-----

I had a few problems getting my new wireless [ML-2165W][] monochrome laser-
printer-thing set-up initially, but I eventually got it working with
a clean install of Ubuntu 13.04 (Raring Ringtail). And---by gum---it's
speedy for a cheap printer!

The following piece records how I got my ML-2165W working. The
intsructions here may not be complete, and probably won't work for you.
After all *it's a printer*. If you have trouble, the best place to go is
the [SULDR homepage](http://bchemnet.com/suldr/), and to start working
your way through the limited literature from there.


Add the <abbr title="Samsung Unified Linux Driver Repository">SULDR</abbr>
---------------------------------------------------------------------------

The [SULDR](http://bchemnet.com/suldr/) handily repackages the official
Samsung printer drivers, and was invaluable in getting my printer
working -- Samsung printer drivers are found lacking with regards to
installation on Linux systems.


```bash
sudo bash -c 'echo "deb http://www.bchemnet.com/suldr/ debian extra" >> /etc/apt/sources.list'
sudo wget -O - http://www.bchemnet.com/suldr/suldr.gpg | sudo apt-key add -
sudo apt-get update
```


Install the Driver Package
--------------------------

I selected the slightly older `suld-driver-4.00.39` package which seems
to be the most direct route to a working ML-2160-series printer driver:

```bash
sudo apt-get install suld-driver-4.00.39
```


Add the Printer to your Network
-------------------------------

Use [WPS](http://en.wikipedia.org/wiki/Wi-Fi_Protected_Setup) to setup a
connection between the printer and your router. Alternatively, find a
Windows PC to run the included installation disc and configure the
network using that.


Configure the Network Printer
-----------------------------

Then I added the printer through the Ubuntu "Printers" utility as an
**[LPD Network
Printer](http://en.wikipedia.org/wiki/Line_Printer_Daemon_protocol)** --
apparently [some Samsung printer/driver combinations can be buggy over
IPP](http://www.bchemnet.com/suldr/forum/index.php?topic=87.0#msg_361).

![Ensure you add the printer using the LPD protocol.](/static/img/screenshot-add-printer-over-lpd.png)

The ML-2165W should then detected as an ML-2160-series printer, after
searching for drivers.


Printing a Page
---------------


I'm not entirely sure whether or not it helped, but I also sent a text
file to the printer through `lp`, [which supposedly fixes common
configuration 'blockages'](http://bchemnet.com/suldr/printing.html#12).
The text file itself never printed, in my case:

```bash
lp -d {printer} {text file}
```

![Yay, it works! And yes it is past my bedtime.](/static/img/ml-2165-cups-test-page.jpg)

Once the printer is fully configured, you should be able to print a CUPS
test-page, as above!


<!-- links -->
[ML-2165W]: http://www.samsung.com/hk_en/consumer/computer-peripherals/printers-multifunction/monochrome-laser-printers/ML-2165/XSS
