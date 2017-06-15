# nomad-chat

This project is intended for the author's learning purposes only and is not appropriate for production-use.

Development
===========

This project uses [Stack](https://www.haskellstack.org/).

```
stack setup
stack build
stack exec nomad-chat-exe
```

Then direct your web browser at `localhost:3000`

Server setup
============

The following was done to initialize the server (TODO: automate this):
* mkdir /opt/nomad-chat
* (extract tar.gz into /opt/, make symlink from /opt/nomad-chat to /opt/nomad-chat-0.0.1.0
* ln -s /opt/nomad-chat/nomad-chat.service /etc/systemd/system/nomad-chat.service
* sudo adduser --system --no-create-home --group --disabled-password --disabled-login nomad-chat

### License

MIT - See LICENSE file.
