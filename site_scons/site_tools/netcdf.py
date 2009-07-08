import os,os.path
# Try for eol_scons.package.Package, which will allow us to build netCDF
# from source if necessary.  If we don't find it, do stuff to disable 
# build-from-source.
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
    netcdf_actions = None

import string
import SCons

# Note that netcdf.inc has been left out of this list, since this
# current setup does not install it.

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

class NetcdfPackage(Package):

    def __init__(self):
        dpf="ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-3.6.2.tar.gz"
        Package.__init__(self, "NETCDF", "INSTALL",
                         netcdf_actions, libs + headers, dpf)

    def require(self, env):
        "Need to add both c and c++ libraries to the environment."
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
            env.AppendUnique(CPPPATH=header.get_dir().get_abspath())
        if self.building:
            env.Append(LIBS=env.File(libs[0]))
            env.Append(LIBS=env.File(libs[1]))
        else:
            env.AppendUnique(LIBPATH=[os.path.join(prefix,'lib')])
            env.AppendUnique(RPATH=[os.path.join(prefix,'lib')])
            env.AppendUnique(LIBPATH=['/usr/lib/netcdf-3'])
            env.Append(LIBS=['netcdf_c++', 'netcdf'])

netcdf_package = NetcdfPackage()

def generate(env):
    netcdf_package.require(env)

def exists(env):
    return True

