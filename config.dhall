-- Base config type
-- Assumed github.com unless `githubAPIEndpoint` is set.

let OrgConfig : Type
        = { githubAPIEndpoint : Optional Text
          , orgName : Text 
          }

let orgConfigs : List OrgConfig = [
    { githubAPIEndpoint = None Text, orgName = "daniel-beard" } 
]

in { orgConfigs }