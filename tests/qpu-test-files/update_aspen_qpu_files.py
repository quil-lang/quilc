"""
Run this script to update the Aspen-*.qpu chipspec files in this
directory. Note this will overwrite existing files.
"""
import json
import datetime
import sys

from pyquil.api import list_quantum_computers
from pyquil.api._devices import get_lattice


def device_to_chipspec(device, version=0.0, timestamp=None):
    return dict(metadata=dict(name=device.name,
                              version=version,
                              timestamp=str(timestamp)),
                isa=device.isa.to_dict(),
                specs=device.specs.to_dict())


def main():
    now = datetime.datetime.today()
    for device in map(get_lattice, list_quantum_computers(qvms=False)):
        print(device.name, file=sys.stderr)
        with open(f'{device.name}.qpu', 'w') as f:
            json.dump(device_to_chipspec(device, timestamp=now), f, indent=2)


if __name__ == "__main__":
    main()
