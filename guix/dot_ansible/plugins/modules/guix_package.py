import json
import os
from ansible.module_utils.basic import AnsibleModule


def main():
    module = AnsibleModule(
        argument_spec=dict(
            load_path=dict(type="str", required=True),
            manifest=dict(type="str", required=True),
            profile=dict(type="str", required=True)
        )
    )
    before = os.path.realpath(module.params['profile'])
    result = module.run_command(f"guix package --fallback --load-path={module.params['load_path']} --manifest={module.params['manifest']}")
    after = os.path.realpath("/home/oleg/.guix-profile")
    result = {
        "msg": {"before": before, "after": after},
        "rc:": result if result[0] != 0 else after,
        "failed": result[0] != 0,
        "changed": before != after,
    }
    module.exit_json(**result)


if __name__ == "__main__":
    main()
