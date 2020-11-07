import json
from ansible.module_utils.basic import AnsibleModule


def main():
    module = AnsibleModule(
        argument_spec=dict(
            commit=dict(type="str", required=True)
        )
    )
    channels_before = module.run_command("guix describe --format=json")
    pull_result = module.run_command("guix pull --channels=/home/oleg/.local/share/chezmoi/dotfiles/channels.scm --commit={}".format(module.params["commit"]))
    channels_after = module.run_command("guix describe --format=json")
    result = {
        "msg": {"before": json.loads(channels_before[1]), "after": json.loads(channels_after[1])},
        "rc:": pull_result if pull_result[0] != 0 else channels_after[0],
        "failed": pull_result[0] != 0,
        "changed": channels_before[1] != channels_after[1],
    }
    module.exit_json(**result)


if __name__ == "__main__":
    main()
