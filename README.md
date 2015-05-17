## GithubNotify
A little tool that uses libnotify to tell you when you receive a new notification
on GitHub.

### Requirements
A notification daemon is required, [dunst](https://www.archlinux.org/packages/?name=dunst) is recommended. This must be installed prior to using github-notify.

You will need the following packages to be able to build/run github-notify. If you are building this from the AUR package: [github-notify](https://aur.archlinux.org/packages/github-notify/), these will be installed from the PKGBUILD.

####Run ( Required to build and run ) :
* libnotify
* notification-daemon

####Build ( Only required to build the package ):
* cabal-install
* ghc
* gtk2hs-buildtools

### Usage
```bash
./github-notify -c <config-file>
```

To use GithubNotify, simply open it as seen above, giving it the path to a config
file.

### Config File
The config file contains a JSON Object, whose only required field is "token".
Set it to your [Personal Access Token](https://github.com/settings/tokens) like so:
```bash
{
	"token": "ace755130ccb1e794dfb50234df9c1847f250530"
}
```
