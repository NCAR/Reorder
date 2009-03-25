import os

vars = Variables('buildOpt.py')
vars.Add(BoolVariable('useNetcdf','Set to yes to compile with NetCDF',False))
vars.Add(PathVariable('ncdir','Path to netcdf install','/usr'))

env = Environment(variables = vars, tools=['default'])

Help(vars.GenerateHelpText(env))

env.AppendUnique(CCFLAGS = ['-O3'])

env.netcdfBin  = os.path.join(env['ncdir'],'bin');
env.netcdfLib  = os.path.join(env['ncdir'],'lib')

# For netCDF RPM packaging which installs the libraries in /usr/lib but the 
# headers in /usr/include/netcdf-3, just set ncdir to '/usr' and this will
# fix up the header location.
if ((env['ncdir'] == '/usr') and os.path.exists('/usr/include/netcdf-3')):
    print
    print 'Using workaround to get headers from /usr/include/netcdf-3'
    print
    env.netcdfInc  = '/usr/include/netcdf-3'
else:
    env.netcdfInc  = os.path.join(env['ncdir'],'include')

env.netcdfLIBS = ['netcdf']


if env['useNetcdf'] : 
    env.AppendUnique(CPPDEFINES = ['NETCDF'])
    env.AppendUnique(CPPPATH = [env.netcdfInc])
    env.AppendUnique(LIBPATH = [env.netcdfLib])
    env.AppendUnique(LIBS    = [env.netcdfLIBS])

env.installLocalLib = '#/lib'
env.installLocalInc = '#/include'
env.installLocalBin = '#/bin'

Export('env')

SConscript('Libraries/SConscript')
SConscript('Applications/SConscript')
