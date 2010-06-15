import os

vars = Variables('buildOpt.py')
vars.Add(BoolVariable('useNetcdf', 'Set to true to compile with NetCDF', False))
vars.Add(BoolVariable('use_m32', 
            "Use the '-m32' flag to build 32-bit binaries (gcc/x86 only!)",
            True))

env = Environment(variables = vars, tools=['default'])

Help(vars.GenerateHelpText(env))

if env['useNetcdf']:
    env.Tool('netcdf')
    env.AppendUnique(CPPDEFINES = ['NETCDF'])

env.AppendUnique(FORTRANFLAGS = ['-fbounds-check'])
    
if env['use_m32']:
    if (env['CC'] == 'gcc'):
        # Put '-m32' onto compilation and linking, since we need to explicitly 
        # build 32-bit binaries, even on 64-bit systems
        env.AppendUnique(CCFLAGS = ['-m32'])
        env.AppendUnique(FORTRANFLAGS = ['-m32'])
        env.AppendUnique(LINKFLAGS = ['-m32'])
    else:
        print 'Ignoring option "use_m32", since we are not using gcc.'
    
env.installLocalLib = '#/lib'
env.installLocalInc = '#/include'
env.installLocalBin = '#/bin'

Export('env')

SConscript('Libraries/SConscript')
SConscript('Applications/SConscript')
