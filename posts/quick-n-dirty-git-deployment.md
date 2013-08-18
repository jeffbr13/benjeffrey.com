---
title: Quick 'n' Dirty Git Deployment
description: A simple solution to deploy source code with remote tracking branches and post-receive hooks.
date: 2013-08-17
---

Git is a D-for-distributed DVCS, meaning that your repository doesn't need
to live at any canonical location.
It can reside entirely on each team-member's hard-drive,
but it's generally a good idea to have a remote copy of your repository for
both backup and collaboration purposes.

If a remote location is all you need, then look into bare repositories, as they
were designed for this.

However, if you want a working directory with a copy of your code directly on
the server, you'll need a setup more like the following.
I'm pretty sure a post-receive hook and a bare repository would be
a safer way to do this, but it works.

<aside>
    **WARNING:** Don't let it *all* hang out on the web -- you should
    ensure your `.htaccess` or alternative webserver configuration
    denies access to `.git` and any other private directories in your repo.
</aside>


## 1: Copy repository to remote server

Make sure your repository is neat, then copy the entire contents of the
working directory to your remote server:

```bash
scp -rC ./ user@server:/var/www/project
```


## 2: Allow pushing to the remote repository

If you try and push up to a remote repo normally, you get an annoying
`Can't push to checked-out branch` error. To allow pushing to the currently
checked-out branch, `cd` to the new repository+working directory on your
remote server, and set the following flag:

```bash
git config --bool receive.denyCurrentBranch false
```


## 3: Create a post-receive hook to checkout pushed changes

Git won't automatically re-checkout the branch you push to, so you'll need
a fancy "post-receive hook" to update your working directory each time you push.

Add the following lines to `.git/hooks/post-receive` in your remote repo:

```bash
# Update working tree after receiving pushed branch
echo "*****************************************************"
echo "Post-receive hook: updating remote working directory"
echo "*****************************************************"
cd ..
env -i git reset --hard
```

then set the executable bit on it:

```bash
chmod +x .git/hooks/post-receive
```


## 4: Set up remote tracking branches

```bash
git remote add REMOTE-NAME user@server:/var/www/html/project
git push REMOTE-NAME BRANCH
```

