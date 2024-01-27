import shutil
import os
import subprocess

# https://stackoverflow.com/questions/14989858/get-the-current-git-hash-in-a-python-script
def get_git_revision_short_hash() -> str:
    return subprocess.check_output(['git', 'rev-parse', '--short', 'HEAD']).decode('ascii').strip()

# Get the origin and destination directories
build_dir = os.path.dirname(os.path.abspath(__file__)) + '/build'
package_dir = os.path.dirname(os.path.abspath(__file__)) + '/package'

# Get the short git hash
git_hash = get_git_revision_short_hash()

# Define default destination filenames
no_limits_name = "VESC_default_no_hw_limits.bin"
default_name = "VESC_default.bin"

# Add directories and targets to the dictionary
# package_dict["group name diplayed in firmware tab of the vesc tool"] = [['.c filename minus the hw_', 'compiled .bin filename']]
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
package_dict["60_MK6_MAX"] = [['60_mk6_max', default_name]]
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
package_dict["A50S_6S"] = [['a50s_v22_6s', default_name]]
package_dict["A50S_6S_HG"] = [['a50s_v22_6s_hg', default_name]]
package_dict["A50S_12S"] = [['a50s_v22_12s', default_name]]
package_dict["A50S_12S_HG"] = [['a50s_v22_12s_hg', default_name]]
package_dict["A50S_V23_6S"] = [['a50s_v23_6s', default_name]]
package_dict["A50S_V23_8S"] = [['a50s_v23_8s', default_name]]
package_dict["A50S_V23_12S"] = [['a50s_v23_12s', default_name]]
package_dict["A50S_V23_20S"] = [['a50s_v23_20s', default_name]]
package_dict["A200S_V2.1"] = [['a200s_v2.1', default_name]]
package_dict["A200S_V2.2"] = [['a200s_v2.2', default_name]]
package_dict["A200S_V3"] = [['a200s_v3', default_name]]
package_dict["A200S_V4"] = [['a200s_v4', default_name]]
package_dict["A200S_V41"] = [['a200s_v41', default_name]]
package_dict["100_250"] = [['100_250', default_name],
                    ['100_250_no_limits', no_limits_name]]
package_dict["100_250_MKIII"] = [['100_250_mkiii', default_name],
                    ['100_250_mkiii_no_limits', no_limits_name]]
package_dict["LUNA_BBSHD"] = [['luna_bbshd', default_name]]
package_dict["LUNA_M600"] = [['luna_m600', default_name]]
package_dict["LUNA_M600_V2"] = [['luna_m600', default_name]]
package_dict["LUNA_M600_V2_Rev5"] = [['luna_m600_Rev5', default_name]]
package_dict["LUNA_M600_V2_Rev5_60V"] = [['luna_m600_Rev5_60V', default_name]]
package_dict["UNITY"] = [['unity', default_name],
                    ['unity_no_limits', no_limits_name]]
package_dict["Cheap_FOCer_2"] = [['Cheap_FOCer_2', default_name],
                    ['Cheap_FOCer_2_no_limits', no_limits_name]]
package_dict["Cheap_FOCer_2_V09"] = [['Cheap_FOCer_2_V09', default_name],
                    ['Cheap_FOCer_2_V09_no_limits', no_limits_name]]
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
package_dict["Little_FOCer"] = [['Little_FOCer', default_name]]
package_dict["Little_FOCer_V3"] = [['Little_FOCer_V3', default_name]]
package_dict["Little_FOCer_V3_1"] = [['Little_FOCer_V3_1', default_name]]
package_dict["Thor300"] = [['Thor_300_20s', default_name]]
package_dict["UXV_SR"] = [['uxv_sr', default_name]]
package_dict["GESC"] = [['gesc', default_name]]
package_dict["Warrior6"] = [['warrior6', default_name]]
package_dict["Raiden7"] = [['raiden7', default_name]]
package_dict["100_500"] = [['100_500', default_name],
                    ['100_500_no_limits', no_limits_name]]
package_dict["75_600"] = [['75_600', default_name],
                    ['75_600_no_limits', no_limits_name]]
package_dict["60v2_alva"] = [['60v2_alva', default_name]]
package_dict["60v2_alva_mk1"] = [['60v2_alva_mk1', default_name]]
package_dict["60v2_alva_mk2"] = [['60v2_alva_mk2', default_name]]
package_dict["gp"] = [['gp', default_name]]
package_dict["60_75"] = [['60_75', default_name],
                    ['60_75_no_limits', no_limits_name]]
package_dict["60_75_mk2"] = [['60_75_mk2', default_name],
                    ['60_75_mk2_no_limits', no_limits_name]]
package_dict["UBOX_SINGLE_75"] = [['ubox_single_75', default_name],
                                  ['ubox_single_75_no_limits', no_limits_name]]
package_dict["UBOX_V1_75_MICRO"] = [['ubox_v1_75_micro', default_name],
                                    ['ubox_v1_75_micro_no_limits', no_limits_name]]
package_dict["UBOX_V1_75_TYPEC"] = [['ubox_v1_75_typec', default_name],
                                    ['ubox_v1_75_typec_no_limits', no_limits_name]]
package_dict["UBOX_V2_75"] = [['ubox_v2_75', default_name],
                              ['ubox_v2_75_no_limits', no_limits_name]]
package_dict["UBOX_SINGLE_100"] = [['ubox_single_100', default_name],
                                   ['ubox_single_100_no_limits', no_limits_name]]
package_dict["UBOX_SINGLE_80"] = [['ubox_single_80', default_name],
                                  ['ubox_single_80_no_limits', no_limits_name]]
package_dict["UBOX_SINGLE_85_200"] = [['ubox_single_85_200', 'default_name.bin'],
                                  ['ubox_single_85_200_no_limits', no_limits_name]]
package_dict["UBOX_V2_100"] = [['ubox_v2_100', default_name],
                               ['ubox_v2_100_no_limits', no_limits_name]]
package_dict["EDU"] = [['edu', default_name],
                    ['edu_no_limits', no_limits_name]]
package_dict["75_300_MKIV"] = [['75_300_mkiv', default_name],
                    ['75_300_mkiv_no_limits', no_limits_name]]
package_dict["60_MK6_HP"] = [['60_mk6_hp', default_name],
                    ['60_mk6_hp_no_limits', no_limits_name]]
package_dict["KA160"] = [['ka160', default_name]]
package_dict["75_100"] = [['75_100', default_name],
                    ['75_100_no_limits', no_limits_name]]
package_dict["75_100_V2"] = [['75_100_V2', default_name],
                    ['75_100_V2_no_limits', no_limits_name],
                    ['75_100_V2_0005ohm', 'VESC_0005ohm.bin']]
package_dict["FSESC75300"] = [['fsesc_75_300', default_name]]
package_dict["GO_FOC_DV6_PRO"] = [['go_foc_dv6_pro', default_name],
                    ['go_foc_dv6_pro_no_limits', no_limits_name]]
package_dict["GO_FOC_G300"] = [['go_foc_g300', default_name],
                    ['go_foc_g300_no_limits', no_limits_name]]
package_dict["GO_FOC_HI200"] = [['go_foc_hi200', default_name],
                    ['go_foc_hi200_no_limits', no_limits_name]]
package_dict["GO_FOC_HV200"] = [['go_foc_hv200', default_name],
                    ['go_foc_hv200_no_limits', no_limits_name]]
package_dict["GO_FOC_M100"] = [['go_foc_m100', default_name],
                    ['go_foc_m100_no_limits', no_limits_name]]
package_dict["SOLO"] = [['solo', default_name],
                    ['solo_no_limits', no_limits_name]]
package_dict["FSESC_75_200_ALU"] = [['fsesc_75_200_alu', default_name],
                    ['fsesc_75_200_alu_no_limits', no_limits_name]]
package_dict["MKSESC_75_100"] = [['mksesc_75_100', default_name],
                    ['mksesc_75_100_no_limits', no_limits_name]]
package_dict["MKSESC_75_100_V2"] = [['mksesc_75_100_v2', default_name],
                    ['mksesc_75_100_v2_no_limits', no_limits_name]]                    
package_dict["MKSESC_75_200_V2"] = [['mksesc_75_200_v2', default_name],
                    ['mksesc_75_200_v2_no_limits', no_limits_name]]
package_dict["MKSESC_84_100_HP"] = [['mksesc_84_100_hp', default_name],
                    ['mksesc_84_100_hp_no_limits', no_limits_name]]                    
package_dict["MKSESC_84_200_HP"] = [['mksesc_84_200_hp', default_name],
                    ['mksesc_84_200_hp_no_limits', no_limits_name]]
package_dict["STR500"] = [['str500', default_name],
                    ['str500_no_limits', no_limits_name]]

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
        destination_full_path = os.path.join(destination_path, destination_file_name)
        origin_file_name = target[0] + '.bin'
        origin_full_path = os.path.join(build_dir, target[0], origin_file_name)

        # Skip firmware that has not been built
        if not os.path.isfile(origin_full_path):
            continue

        # Copy the file
        shutil.copy(origin_full_path, destination_full_path)

        # Replace the stub string with the target specifics
        target_res_string = res_firmwares_string.replace("TARGET_DESTINATION_DIRECTORY", directory).replace("TARGET_DESTINATION_FILENAME", destination_file_name)

        # Add this string to the master Qt resource string
        res_string = res_string + target_res_string

# Print the QRC file
with open(os.path.join(package_dir, 'res_fw.qrc'), 'w') as f:
    print(resource_xml_stub_string.replace("REPLACEABLE_STRING", res_string[:-1]), file=f)

