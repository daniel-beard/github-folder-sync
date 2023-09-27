# github-folder-sync

An easy, fast way to clone many github repositories concurrently by org name, for `github.com`.

## Execute  

* Run `stack run`

## Config

The configuration is specified as a [Dhall](https://learnxinyminutes.com/docs/dhall/) configuration file named `.github-folder-sync`. Either have the config in the current working directory, or override it using: `github-folder-sync --config-file /tmp/adifferentconfig`.
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

github-folder-sync also supports a shorthand syntax within strings for the following fields: 

```
- orgAPIToken 
- orgName
- userName
- userAPIToken
```

For example:

```dhall
orgAPIToken = Some "$GITHUB_FOLDER_SYNC_API_TOKEN"
```

