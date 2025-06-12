# sdl2 cmake project-config input for CMakeLists.txt script

include(FeatureSummary)
set_package_properties(SDL2 PROPERTIES
  URL "https://www.libsdl.org/"
  DESCRIPTION "low level access to audio, keyboard, mouse, joystick, and graphics hardware"
)


####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was SDL2Config.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

set(SDL2_FOUND TRUE)

if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SDL2Targets.cmake")
  include("${CMAKE_CURRENT_LIST_DIR}/SDL2Targets.cmake")
  set(SDL2_SDL2_FOUND TRUE)
endif()
if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SDL2staticTargets.cmake")
  if(ANDROID)
    enable_language(CXX)
  endif()
  include("${CMAKE_CURRENT_LIST_DIR}/SDL2staticTargets.cmake")
  set(SDL2_SDL2-static_FOUND TRUE)
endif()
if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SDL2mainTargets.cmake")
  include("${CMAKE_CURRENT_LIST_DIR}/SDL2mainTargets.cmake")
  set(SDL2_SDL2main_FOUND TRUE)
endif()
if(EXISTS "${CMAKE_CURRENT_LIST_DIR}/SDL2testTargets.cmake")
  include("${CMAKE_CURRENT_LIST_DIR}/SDL2testTargets.cmake")
  set(SDL2_SDL2test_FOUND TRUE)
endif()


include("${CMAKE_CURRENT_LIST_DIR}/sdlfind.cmake")

set(SDL_ALSA OFF)
set(SDL_ALSA_SHARED OFF)
if(SDL_ALSA AND NOT SDL_ALSA_SHARED AND TARGET SDL2::SDL2-static)
  sdlFindALSA()
endif()
unset(SDL_ALSA)
unset(SDL_ALSA_SHARED)


check_required_components(SDL2)

# Create SDL2::SDL2 alias for static-only builds
if(TARGET SDL2::SDL2-static AND NOT TARGET SDL2::SDL2)
  if(CMAKE_VERSION VERSION_LESS "3.18")
    # FIXME: Aliasing local targets is not supported on CMake < 3.18, so make it global.
    add_library(SDL2::SDL2 INTERFACE IMPORTED)
    set_target_properties(SDL2::SDL2 PROPERTIES INTERFACE_LINK_LIBRARIES "SDL2::SDL2-static")
  else()
    add_library(SDL2::SDL2 ALIAS SDL2::SDL2-static)
  endif()
endif()

# For compatibility with autotools sdl2-config.cmake, provide SDL2_* variables.

set(SDL2_PREFIX "${PACKAGE_PREFIX_DIR}")
set(SDL2_EXEC_PREFIX "${PACKAGE_PREFIX_DIR}")
set(SDL2_INCLUDE_DIR "${PACKAGE_PREFIX_DIR}/include/SDL2")
set(SDL2_INCLUDE_DIRS "${PACKAGE_PREFIX_DIR}/include;${PACKAGE_PREFIX_DIR}/include/SDL2")
set(SDL2_BINDIR "${PACKAGE_PREFIX_DIR}/bin")
set(SDL2_LIBDIR "${PACKAGE_PREFIX_DIR}/lib")
set(SDL2_LIBRARIES SDL2::SDL2)
set(SDL2_STATIC_LIBRARIES SDL2::SDL2-static)
set(SDL2_STATIC_PRIVATE_LIBS)

set(SDL2MAIN_LIBRARY)
if(TARGET SDL2::SDL2main)
  set(SDL2MAIN_LIBRARY SDL2::SDL2main)
  list(INSERT SDL2_LIBRARIES 0 SDL2::SDL2main)
  list(INSERT SDL2_STATIC_LIBRARIES 0 SDL2::SDL2main)
endif()

set(SDL2TEST_LIBRARY SDL2::SDL2test)
