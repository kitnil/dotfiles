import json
from ansible.module_utils.basic import AnsibleModule


def main():
    module = AnsibleModule(
        argument_spec=dict(
            channels=dict(type="str", required=True),
        )
    )

    def describe():
        result = module.run_command("guix describe --format=json")
        try:
            return json.loads(result[1])
        except json.JSONDecodeError:
            return result[1]

    before = describe()
    result = module.run_command(
        " ".join(
            [
                "guix",
                "pull",
                "--allow-downgrades",
                f"--channels={module.params['channels']}",
            ]
        )
    )
    after = describe()
    result = {
        "msg": {"before": before, "after": after},
        "rc:": result if result[0] != 0 else after,
        "failed": result[0] != 0,
        "changed": before != after,
    }
    module.exit_json(**result)


if __name__ == "__main__":
    main()
