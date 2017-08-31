# nomad-chat

This project is intended for the author's learning purposes only and is not appropriate for production-use.

Development
===========

This project uses [Stack](https://www.haskellstack.org/).

```
stack setup
stack build
stack exec nomad-chat-exe development
```

Then direct your web browser at `localhost:3000`

Server setup
============

The following was done to initialize the server (TODO: automate this):

    mkdir /opt/nomad-chat
    ln -s /opt/nomad-chat/nomad-chat.service /etc/systemd/system/nomad-chat.service
    sudo adduser --system --no-create-home --group --disabled-password --disabled-login nomad-chat

Then deploy.
 
Deploy
======

TODO: Automate this.

* On the build machine:
```
    ./build.sh
    scp build/nomad-chat-1.0.0.0 my-server:~/
```

* Then on the host
```
    cd /opt
    tar -xf ~/nomad-chat-1.0.0.tar.gz
    ln -f -s /opt/nomad-chat-1.0.0.0 /opt/nomad-chat
```

### License

MIT - See LICENSE file.
