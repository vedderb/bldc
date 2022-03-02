import shutil
import os
import subprocess

# https://stackoverflow.com/questions/14989858/get-the-current-git-hash-in-a-python-script
def get_git_revision_short_hash() -> str:
    return subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('ascii').strip()

# Get the origin and destionation directories
build_dir = os.path.dirname(__file__) + '/../build'
package_dir = os.path.dirname(__file__) + '/../package'

# Get the short git hash
git_hash = get_git_revision_short_hash()

package_targets = ['46', '48',
                    '410', '410_no_limits',
                    '60', '60_no_limits',
                    '60_mk3', '60_mk3_no_limits',
                    '60_mk4', '60_mk4_no_limits',
                    '60_mk5',  '60_mk5_no_limits',
                    'das_rs',
                    '75_300', '75_300_no_limits',
                    '75_300_r2', '75_300_r2_no_limits',
                    '75_300_r3', '75_300_r3_no_limits',
                    'axiom', 'uavc_omega',
                    'hd60', 'hd60_no_limits',
                    'hd75', 'hd75_no_limits',
                    'a50s_6s', 'a50s_12s',
                    'a200s_v2.1', 'a200s_v2.2',
                    '100_250', '100_250_no_limits',
                    'luna_bbshd',
                    'unity', 'unity_no_limits',
                    'stormcore_60d', 'stormcore_60d_no_limits',
                    'stormcore_60dxs', 'stormcore_60dxs_no_limits',
                    'stormcore_60d+', 'stormcore_60d+_no_limits',
                    'stormcore_100d', 'stormcore_100d_no_limits',
                    'stormcore_100d_v2', 'stormcore_100d_v2_no_limits',
                    'stormcore_100dx', 'stormcore_100dx_no_limits',
                    'stormcore_100s', 'stormcore_100s_no_limits',
                    'cheap_focer_2',
                    'little_focer', 'little_focer_no_limits',
                    'uxv_sr', 'gesc',
                    'warrior6', 'raiden7',
                    'ubox_single',
                    '100_500', '100_500_no_limits',
                    '75_600', '75_600_no_limits',
                    '60v2_alva',
                    '60_75', '60_75_no_limits']


# Iterate over each target
for target in package_targets:
    # Set the target filename
    if "_no_limits" in target:
        file_name = "VESC_default_no_limits.bin"
    else:
        file_name = "VESC_default.bin"

    # Set the target destination
    tmp_destination_path = os.path.join(package_dir, target)

    # Remove `_no_limits` string, this forces files with and without limits to be in the same directory
    destination_path = tmp_destination_path.replace("_no_limits", "")

    # Create the destination directory
    os.makedirs(destination_path, exist_ok=True)

    # Copy the file
    shutil.copy(os.path.join(build_dir, target, file_name), os.path.join(destination_path, file_name))
