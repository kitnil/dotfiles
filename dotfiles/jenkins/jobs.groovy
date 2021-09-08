organizationFolder('GitLab Organization Folder') {
    description("https://gitlab.com/wigust")
    displayName('wigust')
    // "Projects"
    organizations {
        gitLabSCMNavigator {
            projectOwner("wigust")
            // credentialsId("i<3GitLab")
            serverName("default")
            // "Traits" ("Behaviours" in the GUI) that are "declarative-compatible"
            traits {
                subGroupProjectDiscoveryTrait() // discover projects inside subgroups
                gitLabBranchDiscovery {
                    strategyId(3) // discover all branches
                }
                // originMergeRequestDiscoveryTrait {
                //     strategyId(1) // discover MRs and merge them with target branch
                // }
                gitLabTagDiscovery() // discover tags
            }
        }
    }
    // "Traits" ("Behaviours" in the GUI) that are NOT "declarative-compatible"
    // For some 'traits, we need to configure this stuff by hand until JobDSL handles it
    // https://issues.jenkins.io/browse/JENKINS-45504
    configure { node ->
        def traits = node / navigators / 'io.jenkins.plugins.gitlabbranchsource.GitLabSCMNavigator' / traits
        // traits << 'io.jenkins.plugins.gitlabbranchsource.ForkMergeRequestDiscoveryTrait' {
        //     strategyId('2')
        //     trust(class: 'io.jenkins.plugins.gitlabbranchsource.ForkMergeRequestDiscoveryTrait$TrustPermission')
        // }
    }
    // "Project Recognizers"
    projectFactories {
        workflowMultiBranchProjectFactory {
            scriptPath 'Jenkinsfile'
        }
    }
    // "Orphaned Item Strategy"
    orphanedItemStrategy {
        discardOldItems {
            daysToKeep(-1)
            numToKeep(-1)
        }
    }
    // "Scan Organization Folder Triggers" : 1 day
    // We need to configure this stuff by hand because JobDSL only allow 'periodic(int min)' for now
    configure { node ->
        node / triggers / 'com.cloudbees.hudson.plugins.folder.computed.PeriodicFolderTrigger' {
            spec('H H * * *')
            interval(86400000)
        }
    }
}
