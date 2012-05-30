
import os, os.path
import string
import SCons

netcdf_headers = string.split("""
ncvalues.h netcdf.h netcdf.hh netcdfcpp.h
""")

headers = [ os.path.join("$OPT_PREFIX","include",f)
            for f in netcdf_headers ]

# We extend the standard netcdf installation slightly by also copying
# the headers into a netcdf subdirectory, so headers can be qualified
# with a netcdf/ path when included.  Aeros does that, for example.

headers.extend ([ os.path.join("$OPT_PREFIX","include","netcdf",f)
                  for f in netcdf_headers ])
libs = string.split("""
$OPT_PREFIX/lib/libnetcdf.a
$OPT_PREFIX/lib/libnetcdf_c++.a
""")

# Try for eol_scons.package.Package, which will allow us to build netCDF
# from source if necessary.  If we don't find it, do stuff to disable 
# build-from-source.

netcdf_actions = None

try:
    from eol_scons.package import Package
    from eol_scons.chdir import MkdirIfMissing
    # Actions if we need to build netCDF from source
    netcdf_actions = [
        "./configure --prefix=$OPT_PREFIX FC= CC=gcc CXX=g++",
        "make",
        "make install",
        MkdirIfMissing("$OPT_PREFIX/include/netcdf") ] + [
        SCons.Script.Copy("$OPT_PREFIX/include/netcdf", h) for h in
        [ os.path.join("$OPT_PREFIX","include",h2) for h2 in netcdf_headers ]
        ]
except ImportError:
    # No build-from-source actions for netCDF if we didn't find the 
    # eol_scons stuff    

    # define a placeholder Package class
    class Package:
        def __init__(self, name, archive_targets, build_actions, 
                     install_targets, default_package_file = None):
            self.building = False
        
        def checkBuild(self, env):
            pass
    # empty command set since we won't build from source

# Note that netcdf.inc has been left out of this list, since this
# current setup does not install it.

class NetcdfPackage(Package):

    def __init__(self):
        dpf="ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-3.6.2.tar.gz"
        Package.__init__(self, "NETCDF", "INSTALL",
                         netcdf_actions, libs + headers, dpf)
        self.settings = {}

    def require(self, env):
        "Need to add both c and c++ libraries to the environment."
        # The netcdf tool avails itself of the settings in the
        # prefixoptions tool, so make sure it gets required first.
        try:
            env.Require('prefixoptions')
        except:
            pass

        if not self.settings:
            self.calculate_settings(env)
        self.apply_settings(env)

    def calculate_settings(self, env):
        Package.checkBuild(self, env)
        if env.has_key('OPT_PREFIX'):
            prefix = env['OPT_PREFIX']
        else:
            prefix = '/usr/local'
        # Look in the typical locations for the netcdf headers, and see
        # that the location gets added to the CPP paths.
        incpaths = [ os.path.join(prefix,'include'),
                     os.path.join(prefix,'include','netcdf'),
                     "/usr/include/netcdf-3",
                     "/usr/include/netcdf" ]
        header = env.FindFile("netcdf.h", incpaths)
        if header:
            self.settings['CPPPATH'] = [ header.get_dir().get_abspath() ]
        if self.building:
            self.settings['LIBS'] = [ env.File(libs[0]), env.File(libs[1]) ]
        else:
            self.settings['LIBPATH'] = [os.path.join(prefix,'lib')]
            self.settings['RPATH'] = [os.path.join(prefix,'lib')]
            self.settings['LIBPATH'].append('/usr/lib/netcdf-3')

            # Now try to check whether the HDF libraries are needed
            # explicitly when linking with netcdf.  Use a cloned
            # Environment so Configure does not modify the original
            # Environment.  Also, reset the LIBS so that libraries in
            # the original Environment do not affect the linking.  All
            # the library link check needs is the netcdf-related
            # libraries.

            clone = env.Clone()
            libs = ['netcdf_c++', 'netcdf']
            clone.Replace(LIBS=libs)
            conf = clone.Configure()
            if not conf.CheckLib('netcdf'):
                libs.append(['hdf5_hl', 'hdf5', 'bz2'])
                clone.Replace(LIBS=libs)
                if not conf.CheckLib('netcdf'):
                    msg = "Failed to link to netcdf both with and without"
                    msg += " explicit HDF libraries.  Check config.log."
                    raise SCons.Errors.StopError, msg
            self.settings['LIBS'] = libs
            conf.Finish()

    def apply_settings(self, env):
        if self.settings.has_key('CPPPATH'):
            env.AppendUnique(CPPPATH=self.settings['CPPPATH'])

        env.Append(LIBS=self.settings['LIBS'])
        if not self.building:
            env.AppendUnique(LIBPATH=self.settings['LIBPATH'])
            env.AppendUnique(RPATH=self.settings['RPATH'])



# Background on Configure check for netcdf linking: The first attempt
# directly used the Environment passed in.  That works as long as the
# Environment does not already contain dependencies (such as internal
# project libraries) which break the linking.  The other option was to
# create a brand new Environment.  However, if this tool is a global
# tool, then there will be infinite recursion trying to create the new
# Environment.  So the current approach clones the Environment, but
# then resets the LIBS list on the assumption that none of those
# dependencies are needed to link with netcdf.

netcdf_package = NetcdfPackage()

def generate(env):
    netcdf_package.require(env)

def exists(env):
    return True

