---
title: CKAN ansible playbook
date: 2014-05-05
---

This Ansible playbook provisions a complete CKAN installation,
following [the official CKAN package installation guide][1]
on Ubuntu 12.04 LTS (Precise).

It also configures some unattended upgrades, enables passwordless sudo, and may need to be executed twice.

I've just thrown it together, and the new Ubuntu LTS release (14.04) has just come out, so please check the playbook
before you run it.

Comments welcome on [the Github gist](https://gist.github.com/jeffbr13/08751e42c9355cc44f5d).

```yaml
---
- hosts: all

  sudo: yes

  vars:
    - db_name: ckan_default
    - db_user: ckan_default
    - db_password: ckan_default
    - ckan_package_filename: 'python-ckan_2.2_amd64.deb'

  tasks:

    - name: Update apt cache
      apt: update_cache=yes

    - name: Upgrade all safe packages
      apt: upgrade=safe

    - name: Install unattended upgrades (Debian/Ubuntu only)
      apt: pkg=unattended-upgrades state=installed
      when: ansible_distribution == 'Debian' or ansible_distribution == 'Ubuntu'

    - name: 'Make sure unattended-upgrades only installs from $ubuntu_release-security'
      lineinfile: 'dest=/etc/apt/apt.conf.d/50unattended-upgrades regexp="$ubuntu_release-updates" state=absent'
      when: ansible_distribution == 'Ubuntu'

    - name: Install necessities and nice-to-haves
      apt: pkg={{ item }} state=installed
      with_items:
        - acl
        - apache2
        - apt-transport-https
        - apticron
        - aptitude
        - build-essential
        - debian-goodies
        - fail2ban
        - git
        - htop
        - iftop
        - iotop
        - libapache2-mod-wsgi
        - libpq5
        - nginx
        - postgresql
        - python
        - python-psycopg2
        - python-pycurl
        - python-software-properties
        - screen
        - solr-jetty
        - sudo
        - update-notifier-common
        - wget
      when: ansible_pkg_mgr == 'apt'

    - name: Enable passwordless sudo
      lineinfile: 'dest=/etc/sudoers regexp="sudo ALL=NOPASSWD: ALL" line="%sudo ALL=NOPASSWD: ALL" state=present'

    - name: Download CKAN package
      get_url: 'url="http://packaging.ckan.org/{{ ckan_package_filename }}" dest=/tmp/{{ ckan_package_filename }}'

    - name:  Install CKAN package
      command: 'dpkg --skip-same-version -i /tmp/{{ ckan_package_filename }}'
      # http://stackoverflow.com/questions/19127493/in-ansible-how-do-you-prevent-a-dpkg-installation-task-to-notify-a-changed-stat
      register: ckan_installed
      changed_when: "'already installed' not in ckan_installed.stderr"
      notify:
        - Restart Apache
        - Restart Nginx

    # Jetty & Solr
    - name: Set Jetty to start on boot
      lineinfile: 'dest=/etc/default/jetty regexp=^NO_START line="NO_START=0"'

    - name: Set Jetty host to localhost
      lineinfile: 'dest=/etc/default/jetty regexp=^JETTY_HOST line="JETTY_HOST=127.0.0.1"'

    - name: Set Jetty to port 8983
      lineinfile: 'dest=/etc/default/jetty regexp=^JETTY_PORT line="JETTY_PORT=8983"'

    - name: Set Jetty to use system java
      lineinfile: 'dest=/etc/default/jetty regexp=JAVA_HOME line="JAVA_HOME=/usr/lib/jvm/java-6-openjdk-amd64/"'
      notify: Start Jetty

    - name: Remove CKAN schema file
      file: path=/etc/solr/conf/schema.xml state=absent
      when: ckan_installed.changed

    - name: Ensure CKAN uses provided schema file
      file: path=/etc/solr/conf/schema.xml src=/usr/lib/ckan/default/src/ckan/ckan/config/solr/schema.xml state=link
      notify: Restart Jetty

    - name: Set CKAN Solr server address
      lineinfile: 'dest=/etc/ckan/default/production.ini regexp=solr_url line=solr_url=http://127.0.0.1:8983/solr'

    # Postgres
    - name: Ensure CKAN database is created
      postgresql_db: 'name={{ db_name }}'
      sudo_user: postgres

    - name: Ensure CKAN database user can access CKAN database
      postgresql_user: 'db={{ db_name }} name={{ db_user }} password={{ db_password }} priv=ALL'
      sudo_user: postgres

    - name: Minimise CKAN database user priveliges
      postgresql_user: 'name={{ db_user }} role_attr_flags=NOSUPERUSER,NOCREATEROLE,NOCREATEDB'
      sudo_user: postgres

    - name: Set CKAN database server address
      lineinfile: 'dest=/etc/ckan/default/production.ini regexp=sqlalchemy.url line="sqlalchemy.url = postgresql://{{ db_user }}:{{ db_password }}@localhost/{{ db_name }}?sslmode=disable"'

    - name: Ensure database is initialised
      command: ckan db init
      notify:
        - Restart Apache
        - Restart Nginx

    - name: Remove Repoze.who configuration file for CKAN
      file: path=/usr/lib/ckan/default/src/ckan/who.ini state=absent
      when: ckan_installed.changed

    - name: Link Repoze.who configuration file for CKAN
      file: 'path=/usr/lib/ckan/default/src/ckan/who.ini src=/etc/ckan/default/who.ini state=link'

    # DataStore
    - name: Ensure DataStore database exists
      postgresql_db: name=datastore_default owner=ckan_default
      sudo_user: postgres

    - name: Ensure CKAN database user owns DataStore database
      postgresql_user: 'db=datastore_default name=ckan_default password={{ db_password }} priv=ALL'
      sudo_user: postgres

    - name: Ensure DataStore database user exists
      postgresql_user: 'name=datastore_default password={{ db_password }}'
      sudo_user: postgres

    - name: Set DataStore database server write address
      lineinfile: 'dest=/etc/ckan/default/production.ini regexp="ckan.datastore.write_url" line="ckan.datastore.write_url = postgresql://ckan_default:{{ db_password }}@localhost/datastore_default"'

    - name: Set DataStore database server read address
      lineinfile: 'dest=/etc/ckan/default/production.ini regexp="ckan.datastore.read_url" line="ckan.datastore.read_url = postgresql://datastore_default:{{ db_password }}@localhost/datastore_default"'

    - name: Set DataStore database permissions
      command: ckan datastore set-permissions postgres

    # FileStore
    - name: Ensure FileStore directory exists
      file: path=/var/lib/ckan/default owner=www-data state=directory

    - name: Set FileStore directory path
      lineinfile: 'dest=/etc/ckan/default/production.ini regexp="ckan.storage_path" line="ckan.storage_path = /var/lib/ckan/default"'

    - name: Ensure Apache can write to FileStore directory
      acl: name=/var/lib/ckan/default entity=www-data etype=user permissions=u+rwx
      notify: Restart Apache


  handlers:
    - name: Restart Apache
      service: name=apache2 state=restarted

    - name: Restart Nginx
      service: name=nginx state=restarted

    - name: Start Jetty
      service: name=jetty state=started

    - name: Restart Jetty
      service: name=jetty state=restarted
```

[1]: http://docs.ckan.org/en/latest/maintaining/installing/install-from-package.html
