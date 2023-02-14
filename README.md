# github-folder-sync

An easy, fast way to clone many github repositories concurrently by org name, for `github.com`.

## Execute  

* Run `stack run`

## Config

The configuration is specified as a [Dhall](https://learnxinyminutes.com/docs/dhall/) configuration file named `.github-folder-sync`. The config resolution works much the same way as npm `package.json` resolution does, i.e. we check the current folder, then traverse up each path component until we find a config file.

A minimal configuration looks like this:

```dhall
let orgConfigs = [
    { orgAPIToken = None Text
    , orgName = "daniel-beard" 
    , ignoringOrgRepos = ["nothing"]
    } 
]

in { orgConfigs }
```

### Env vars

It's possible to expand env vars within a `.github-folder-sync` file by either using the built-in Dhall way:

```dhall
let orgConfigs = [
    { orgAPIToken = Some env:GITHUB_FOLDER_SYNC_API_TOKEN 
    , orgName = "daniel-beard" 
    , ignoringOrgRepos = ["nothing"]
    } 
]

in { orgConfigs }
```

github-folder-sync also supports a shorthand syntax within strings for the following fields: `githubAPIEndpoint`, `githubAPIToken`, `orgName` 

```
orgAPIToken = Some "$GITHUB_FOLDER_SYNC_API_TOKEN"
```

### API Endpoints

- The api endpoint is assumed to point at `github.com` unless it contains a value.
