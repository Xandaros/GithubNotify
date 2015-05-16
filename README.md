## GithubNotify
A little tool that uses libnotify to tell you when you receive a new notification
on GitHub.

### Usage
./github-notify -c <config-file>

To use GithubNotify, simply open it as seen above, giving it the path to a config
file.

### Config File
The config file contains a JSON Object, whose only required field is "token".
Set it to your [Personal Access Token](https://github.com/settings/tokens) like so:
```
{
	"token": "ace755130ccb1e794dfb50234df9c1847f250530"
}
```
