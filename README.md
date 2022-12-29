# github-folder-sync

An easy, fast way to clone many github repositories concurrently by org name, for `github.com` or `GitHub Enterprise`.

## Execute  

* Run `stack run`

## Config

The configuration is specified as a [Dhall](https://learnxinyminutes.com/docs/dhall/) configuration file named `.github-folder-sync`. The config resolution works much the same way as npm `package.json` resolution does, i.e. we check the current folder, then traverse up each path component until we find a config file.

A minimal configuration looks like this:

```dhall
let orgConfigs = [
    { githubAPIEndpoint = None Text
    , githubAPIToken = None Text
    , orgName = "daniel-beard" 
    , ignoringRepos = ["nothing"]
    } 
]

in { orgConfigs }
```

You can expand env vars within `githubAPIEndpoint`, `githubAPIToken`, `orgName` fields, like this:

```
githubAPIToken = Some "$GITHUB_FOLDER_SYNC_API_TOKEN"
```

- The api endpoint is assumed to point at `github.com` unless it contains a value.

The type of the configuration is (`.github-folder-sync`):

```dhall
let OrgConfig : Type
        = { githubAPIEndpoint : Optional Text
          , githubAPIToken : Optional Text
          , orgName : Text 
          , ignoringRepos : List Text
          }
let orgConfigs : List OrgConfig = [...]
```
