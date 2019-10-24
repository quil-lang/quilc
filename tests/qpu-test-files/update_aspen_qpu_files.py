"""
Run this script to update the Aspen-*.qpu chipspec files in this
directory. Note this will overwrite existing files.
"""
import json
import datetime
import sys
import os

from pyquil.api import list_quantum_computers
from pyquil.api._devices import get_lattice, list_devices


def device_to_chipspec(device, version=0.0, timestamp=None):
    return dict(metadata=dict(name=device.name,
                              version=version,
                              timestamp=str(timestamp)),
                isa=device.isa.to_dict(),
                specs=device.specs.to_dict())


# Device/Date/Lattice.qpu
qpu_dir = "{}/{}/{}.qpu"


def main():
    now = str(datetime.datetime.today())
    print(list_devices())
    for device in list_devices():
        os.makedirs(os.path.join(device, now), exist_ok=True)

    for lattice in map(get_lattice, list_quantum_computers(qvms=False)):
        print(lattice.name, file=sys.stderr)
        with open(qpu_dir.format(device, now, lattice.name), 'w') as f:
            json.dump(device_to_chipspec(lattice, timestamp=now), f, indent=2)

    os.unlink(f"{device}/latest")
    os.symlink(f"{now}", f"{device}/latest")


if __name__ == "__main__":
    main()
