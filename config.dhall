-- Base config type
-- Assumed github.com unless `githubAPIEndpoint` is set.

let OrgConfig : Type
        = { githubAPIEndpoint : Optional Text
          , githubAPIToken : Optional Text
          , orgName : Text 
          , ignoringRepos : List Text
          }

let orgConfigs : List OrgConfig = [
    { githubAPIEndpoint = None Text
    , githubAPIToken = None Text
    , orgName = "daniel-beard" 
    , ignoringRepos = ["Chippy"] } 
]

in { orgConfigs }