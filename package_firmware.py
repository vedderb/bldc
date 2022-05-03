import shutil
import os
import subprocess

# https://stackoverflow.com/questions/14989858/get-the-current-git-hash-in-a-python-script
def get_git_revision_short_hash() -> str:
    return subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('ascii').strip()

# Get the origin and destionation directories
build_dir = os.path.dirname(os.path.abspath(__file__)) + '/build'
package_dir = os.path.dirname(os.path.abspath(__file__)) + '/package'

# Get the short git hash
git_hash = get_git_revision_short_hash()

# Define default destination filenames
no_limits_name = "VESC_default_no_hw_limits.bin"
default_name = "VESC_default.bin"

# Add directories and targets to the dictionary
package_dict = {}
package_dict["46_o_47"] = [['46', default_name],
                    ['46_33k', 'VESC_33k.bin'],
                    ['46_0005ohm', 'VESC_0005ohm.bin']]
package_dict["48"] = [['48', default_name]]
package_dict["410_o_411_o_412"] = [['410', default_name],
                    ['410_no_limits', no_limits_name],
                    ['410_0005ohm', 'VESC_0005ohm.bin'],
                    ['410_005ohm', 'VESC_005ohm.bin']]
package_dict["60"] = [['60', default_name],
                    ['60_no_limits', no_limits_name]]
package_dict["60_MK3"] = [['60_mk3', default_name],
                    ['60_mk3_no_limits', no_limits_name]]
package_dict["60_MK4"] = [['60_mk4', default_name],
                    ['60_mk4_no_limits', no_limits_name]]
package_dict["60_MK5"] = [['60_mk5', default_name],
                    ['60_mk5_no_limits', no_limits_name]]
package_dict["60_MK6"] = [['60_mk6', default_name],
                    ['60_mk6_no_limits', no_limits_name]]
package_dict["DAS_RS"] = [['das_rs', default_name]]
package_dict["75_300"] = [['75_300', default_name],
                    ['75_300_no_limits', no_limits_name]]
package_dict["75_300_R2"] = [['75_300_r2', default_name],
                    ['75_300_r2_no_limits', no_limits_name]]
package_dict["75_300_R3"] = [['75_300_r3', default_name],
                    ['75_300_r3_no_limits', no_limits_name]]
package_dict["AXIOM"] = [['axiom', default_name]]
package_dict["UAVC_OMEGA"] = [['uavc_omega', default_name]]
package_dict["HD60"] = [['hd60', default_name],
                    ['hd60_no_limits', no_limits_name]]
package_dict["HD75"] = [['hd75', default_name],
                    ['hd75_no_limits', no_limits_name]]
package_dict["A50S_6S"] = [['a50s_6s', default_name]]
package_dict["A50S_12S"] = [['a50s_12s', default_name]]
package_dict["A200S_v2.1"] = [['a200s_v2.1', default_name]]
package_dict["A200S_v2.2"] = [['a200s_v2.2', default_name]]
package_dict["100_250"] = [['100_250', default_name],
                    ['100_250_no_limits', no_limits_name]]
package_dict["LUNA_BBSHD"] = [['luna_bbshd', default_name]]
package_dict["UNITY"] = [['unity', default_name],
                    ['unity_no_limits', no_limits_name]]
package_dict["Cheap_FOCer_2"] = [['Cheap_FOCer_2', default_name]]
package_dict["STORMCORE_60D"] = [['stormcore_60d', default_name],
                    ['stormcore_60d_no_limits', no_limits_name]]
package_dict["STORMCORE_60Dxs"] = [['stormcore_60dxs', default_name],
                    ['stormcore_60dxs_no_limits', no_limits_name]]
package_dict["STORMCORE_60D+"] = [['stormcore_60d+', default_name],
                    ['stormcore_60d+_no_limits', no_limits_name]]
package_dict["STORMCORE_100D"] = [['stormcore_100d', default_name],
                    ['stormcore_100d_no_limits', no_limits_name]]
package_dict["STORMCORE_100D_V2"] = [['stormcore_100d_v2', default_name],
                    ['stormcore_100d_v2_no_limits', no_limits_name]]
package_dict["STORMCORE_100DX"] = [['stormcore_100dx', default_name],
                    ['stormcore_100dx_no_limits', no_limits_name]]
package_dict["STORMCORE_100S"] = [['stormcore_100s', default_name],
                    ['stormcore_100s_no_limits', no_limits_name]]
package_dict["Little_FOCer"] = [['Little_FOCer', default_name],
                    ['Little_FOCer_no_limits', no_limits_name]]
package_dict["UXV_SR"] = [['uxv_sr', default_name]]
package_dict["GESC"] = [['gesc', default_name]]
package_dict["Warrior6"] = [['warrior6', default_name]]
package_dict["Raiden7"] = [['raiden7', default_name]]
package_dict["UBOX_SINGLE"] = [['ubox_single', default_name]]
package_dict["100_500"] = [['100_500', default_name],
                    ['100_500_no_limits', no_limits_name]]
package_dict["75_600"] = [['75_600', default_name],
                    ['75_600_no_limits', no_limits_name]]
package_dict["60v2_alva"] = [['60v2_alva', default_name]]
package_dict["60v2_alva_mk2"] = [['60v2_alva_mk2', default_name]]
package_dict["60_75"] = [['60_75', default_name],
                    ['60_75_no_limits', no_limits_name]]

# This is the firmware stub string
res_firmwares_string = '        <file>TARGET_DESTINATION_DIRECTORY/TARGET_DESTINATION_FILENAME</file>\n'

# This is the XML stub string
resource_xml_stub_string = '''
<RCC>
   <qresource prefix="/res/firmwares/">
REPLACEABLE_STRING
   </qresource>
</RCC>
'''

# Declare an empty string
res_string = ""

# Iterate over all directories in the dictionary
for directory in package_dict:

    # Set the destination path
    destination_path = os.path.join(package_dir, directory)

    # Create the destination directory
    os.makedirs(destination_path, exist_ok=True)

    # Iterate over each target
    for target in package_dict[directory]:
        # Shorthand variable
        destination_file_name = target[1]
        origin_file_name = target[0] + '.bin'

        # Copy the file
        shutil.copy(os.path.join(build_dir, target[0], origin_file_name), os.path.join(destination_path, destination_file_name))

        # Replace the stub string with the target specifics
        target_res_string = res_firmwares_string.replace("TARGET_DESTINATION_DIRECTORY", directory).replace("TARGET_DESTINATION_FILENAME", destination_file_name)

        # Add this string to the master Qt resource string
        res_string = res_string + target_res_string

# Print the QRC file
with open(os.path.join(package_dir, 'res_fw.qrc'), 'w') as f:
    print(resource_xml_stub_string.replace("REPLACEABLE_STRING", res_string[:-1]), file=f)

