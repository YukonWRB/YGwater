''' Created on Mon Jan 30 2016
 @author Jan Talsma
 amended by Micha Werner
 amended by Dave Casson
 upgraded to Python3 Dave CAasson
 adapted as template for SWE download by Everett Snieder

 Script to harvest ERA5 land surface data from the Copernicus Climate Data Store (CDS) using the CDS API.
 
 Can be run with command line arguments
 1. Name of XML run file (exported from FEWS - contains paths)
 2. Model to download [RDPS; GDPS; HRDPS; REPS; GEPS] #TODO: replace
 3. Model time step (in hours)
 4. Maximum lead time to download (in hours)
 5. Option to use multi-threading or to download sequentially

'''

import os
import time
import optparse as op
import datetime as dt
import urllib3
import fews_adapt_py3 as fa
import threading
import socket
import certifi

global NumFilesDownloaded
global FilesFailed

timeout = 10
socket.setdefaulttimeout(timeout)

# Don't remove this line.  The idna encoding
# is used by getaddrinfo when dealing with unicode hostnames,
# and in some cases, there appears to be a race condition
# where threads will get a LookupError on getaddrinfo() saying
# that the encoding doesn't exist.  Using the idna encoding before
# running any CLI code (and any threads it may create) ensures that
# the encodings.idna is imported and registered in the codecs registry,
# which will stop the LookupErrors from happening.
# See: https://bugs.python.org/issue29288
u''.encode('idna')



####################################################################################################
def getRunInfo(settingsDict, runInfoFile,logger):
    ''' Function to retrieve run time information from FEWS '''
    
    logger.info('Loading run time information from run info file')
    workDir   = fa.getElementFromRuninfo(runInfoFile, 'workDir')
    startTime = fa.getStartTimefromRuninfo(runInfoFile)
    endTime   = fa.getEndTimefromRuninfo(runInfoFile)
    
    # add to settings dict, not used
    settingsDict['startTime'] = startTime
    settingsDict['endTime']   = endTime
    settingsDict['workDir']   = workDir
    

    settingsDict['diagnosticFile']  = fa.getElementsFromRuninfo(runInfoFile, 'outputDiagnosticFile')
    settingsDict['destinationDir'] =  fa.getKeyValueFromRunInfo(runInfoFile, 'destinationDir', True)
    

    return settingsDict



def start_thread(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads):
    ''' Function implement threading for data download'''
    global numFilesDownloaded
    global FilesFailed
    threadLimiter.acquire()
    
    thread = threading.Thread(target=data_download, args=(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads))
    thread.start()
       
    return thread

def data_download(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads):
    ''' Function to download data from url, called by threading'''
    outputFile = os.path.join(outputDir,filename)

    if os.path.isfile(outputFile):
        logger.info('File %s already exists. Download cancelled' %filename)
        if useThreading:
            threadLimiter.release()
    else:
        try:
            http = urllib3.PoolManager(timeout=60, retries=3, num_pools = maximumNumberOfThreads, cert_reqs='CERT_REQUIRED', ca_certs=certifi.where())
            response = http.request('GET',url,preload_content=False)
            with open(outputFile, 'wb') as out:
                while True:
                    data = response.read(1024)
                    if not data:
                        break
                    out.write(data)

            response.release_conn()
            url = url.replace('&','&amp;')
            logger.info('Downloading url complete [%s]' %url)
            global NumFilesDownloaded
            NumFilesDownloaded +=1
        except urllib3.exceptions.NewConnectionError:
            logger.error('Downloading url failed [%s]' %url)
            FilesFailed.append(filename)
        except urllib3.exceptions.HTTPError:
            logger.error('Downloading url failed [%s]' %url)
            FilesFailed.append(filename)

    if useThreading:
        #Log Current number of threads
        logger.info('Maximum Number Concurrent of Threads Allowed = %d, Number of Threads Active = %d' %(maximumNumberOfThreads, threading.active_count()-1))
        threadLimiter.release()
    return True

def mainscript():
    
    start = time.time()
    txtLogFile   = os.path.normpath('get_ec_forecast.log')
    
    logger = fa.setlogger(txtLogFile,'log')
    logger.info('Started Environment Canada download script at %s', str(dt.datetime.fromtimestamp(start)))
    
    # get command line arguments - this is the name of the run fike
    cmdLineArgs = op.OptionParser()
    cmdLineArgs.add_option('--runInfoFile',  '-r',  default='none') 
    cmdLineArgs.add_option('--dataset',        '-m',  default='reannalysis-era5-land') 
    #cmdLineArgs.add_option('--leadTime',     '-l',  default='48')
    cmdLineArgs.add_option('--parameter',    '-p',  default='snow_depth_water_equivalent') 
    cmdLineArgs.add_option('--latency',        '-l',  default='5')
    cmdLineArgs.add_option('--nDays','-h',  default='7') # number of days to look back
    cmdLineArgs.add_option('--frequency','-h',  default='24') # factor of 24
    cmdLineArgs.add_option('--threading',    '-t',  default='1')  
    cmdLineArgs.add_option('--maxThreads',    '-n',  default='10')  
    cmdLineArgs.add_option('--extent',    '-e',  default='yukon')  
    
    cmdOptions, cmdArguments = cmdLineArgs.parse_args()
    runInfoFile = cmdOptions.runInfoFile
    dataset = cmdOptions.dataset
    nDays = int(cmdOptions.nDays)
    frequency = int(cmdOptions.frequency)
    latency = int(cmdOptions.latency)
    parameterString = cmdOptions.parameter
    urlBase = cmdOptions.urlBase
    threadingOpt = int(cmdOptions.threading)
    maximumNumberOfThreads = int(cmdOptions.maxThreads)
    extent = cmdOptions.extent

    # interpret option whether to use threading
    useThreading=False
    threadLimiter = threading.BoundedSemaphore(maximumNumberOfThreads) #Additional Variable to Control Threading, defined even if threading not used
    
    if threadingOpt==1: 
        useThreading=True
        logger.info('Threading enabled with maximum number of threads =  %d'  %maximumNumberOfThreads)
        
    settingsDict = dict()

    # CDS credentials and hard-coded extents
    settingsDict['CDS_URL'] = "https://cds.climate.copernicus.eu/api/v2"
    settingsDict['CDS_KEY'] = "5815cfa9-2642-46bd-9a7f-9ac2099b32f4"
    settingsDict['CDS_USER'] = "everett.snieder@gmail.com"

    settingsDict["EXTENT"] = {}
    settingsDict["EXTENT"]["yukon"] = [70, -140, 55, -120]
    settingsDict["EXTENT"]["canada"] = [70, -140, 30, -50]
    settingsDict["EXTENT"]["alaska"] = [70, -170, 50, -130]


    if 'none' in runInfoFile:
        logger.warning('No command line provided -downloading regional model for 48 hours') # TODO
        #if not command line is provided, download regional model
        settingsDict['destinationDir'] = '.'
    else:
        # in this case a run file is provided as well as other command line arguments        
        settingsDict = getRunInfo(settingsDict, runInfoFile,logger)   


    xmlLog = False    
    xmlLogFile = os.path.normpath('get_era5_reanalysis_log.xml')
    if 'diagnosticFile' in settingsDict:
        xmlLog = True
        #xmlLogFile = settingsDict['diagnosticFile']  # this does not seem to work! Assigning logFile name

    outputDir = settingsDict['destinationDir']    

    logger.info(xmlLogFile)
    
    if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file


    # check if running for a valid model identifier - if not then bomb
    # TODO: populate this with complete list of params
    PARAMETER_CHOICES = ['snow_depth_water_equivalent','snow_density']
    if parameter not in PARAMETER_CHOICES:
        logger.error(f'Unrecognized era5 parameter identifier code [{parameter}]  use one of {PARAMETER_CHOICES}' )
        if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file
        quit() 

    
    # allocate settings depending on the model chosen
    #timeStep  = settingsDict['%s_TIMESTEP' %model]
    
    #interval  = settingsDict['%s_INTERVAL' %model]
    
    # assign URL if not provided in command line
    #if 'none' in urlBase:
    #    urlBase = settingsDict['%s_URL' %model]
    
    # reference time is utc+00 - configurable delay (cfs data is in utc+00)

    refDate = dt.datetime.now(dt.timezone.utc)-dt.timedelta(0,latency*24*3600)
    startDate = refDate - dt.timedelta(days=nDays)

    timesteps = 24/frequency

    # Generate a datetime array from startDate to refDate incrementing by timesteps (in hours)
    datetime_array = [startDate + dt.timedelta(hours=i * timesteps) for i in range(int((refDate - startDate).total_seconds() // (timesteps * 3600)) + 1)]


    logger.info(f'Downloading era5 reanalysis ({parameter}) for period of {startDate} to {refDate} at a {timesteps} hour interval')
    if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file

    # list with variables to download - this may be a comma separated list
    parameters  = parameterString.split(',')
    # 'TMP_TGL_2','APCP_SFC_0']

    #logger.debug('Downloading parameters  [%s] for lead times [%d] to [%d] at [%d] intervals' %(parameterString, firstLeadTime, leadTime, timeStep))
    #if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file
  

    #Create empty list for threads
    threads = list()
    
    global NumFilesDownloaded
    global FilesFailed
    NumFilesDownloaded = 0
    FilesFailed = list()
     
    for DateTime in datetime_array:

        for parameter in parameters:
    
            if 'RDPS' in model:  filename = 'CMC_reg_' + parameter + '_ps10km_' + dStr + hStr + '_P' + leadTime +'.grib2'
            if 'GDPS' in model:  filename = 'CMC_glb_' + parameter + '_latlon.15x.15_' + dStr + hStr + '_P' + leadTime +'.grib2'
    #        if 'HRDPS' in model: filename = 'CMC_hrdps_' + extent + '_' + parameter + '_ps2.5km_' + dStr + hStr + '_P' + leadTime +'-00.grib2'
            if 'HRDPS' in model: filename = dStr + 'T' + hStr + 'Z_MSC_HRDPS_' + parameter + '_RLatLon0.0225_PT' + leadTime + 'H.grib2'
            if 'REPS' in model: filename = 'CMC-reps-srpe-raw_' + parameter + '_ps15km_' + dStr + hStr + '_P' + leadTime +'_allmbrs.grib2'
            if 'GEPS' in model: filename = 'CMC_geps-raw_' + parameter + '_latlon0p5x0p5_' + dStr + hStr + '_P' + leadTime + '_allmbrs.grib2'
            url = urlBase + hStr + '/' + leadTime + '/' + filename
            print('this is leadtime '+leadTime)


            
            if useThreading:
                print('start ' + filename)
                threads.append(start_thread(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads))
            else:
                data_download(url,outputDir,filename,logger,useThreading,threadLimiter,maximumNumberOfThreads)

    if useThreading: [thread.join() for thread in threads]
    
    logger.info('Completed downloading %d files in %d seconds'%(NumFilesDownloaded, time.time() - start))
    
    for failedfile in FilesFailed:
        logger.error('File failed to download: %s' %failedfile)
        
    if xmlLog: fa.log2xml(txtLogFile,xmlLogFile) # this convers text log file to fews diagnostics file


####################################################################################################    
if __name__ == '__main__':
    ### this is not executed when script is imported from another script
    mainscript()
    