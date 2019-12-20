# import libraries
import os
import json


#initialize the result list
res=[]

# get the folder with the data
# use dynamic addressing of path (only the structure of the project is identical)
pathData=os.path.dirname(os.path.dirname(os.getcwd())) # get the project path
pathData=pathData+"/03_computedData/07_deploymentData" # go to the deployment data in computed data

# loop over the filter directories in 07_deploymentData
for i in os.listdir(pathData):
	pathFilter=pathData+"/"+i
	if (os.path.isdir(pathFilter) and i.startswith('filter')):
		filterData={}
		filterData["filter"]=i

		#loop in every filter directory over the types of models
		for j in os.listdir(pathFilter):
			if os.path.isdir(pathFilter+"/"+j):
				filterData["model"]=j
				pathModel=pathFilter+"/"+j

				# loop over all evaluation variables in the model
				for k in os.listdir(pathModel):
					if os.path.isdir(pathModel+"/"+k):
						pathPara=pathModel+"/"+k

						# get all json files
						jsonFiles=[]
						for l in os.listdir(pathPara):
							if l.endswith(".json"):
								jsonFiles.append(l)

						# read in the evalutation
						if k=="calculateAreaUnderCurve":
							with open(pathPara+"/"+jsonFiles[0]) as json_file:
								data = json.load(json_file)
								filterData["auc"]=data
						elif k=="variable_importance":
							with open(pathPara+"/"+jsonFiles[0]) as json_file:
								data = json.load(json_file)
								filterData["variableImportance"]=data
						elif k=="prediction_plots":
							data=[]
							for jfile in jsonFiles:
								with open(pathPara+"/"+jfile) as json_file:
									data.append(json.load(json_file))
							filterData["featureVsPrediction"]=data
						elif k=="classDistribution":
							data=[]
							for jfile in jsonFiles:
								with open(pathPara+"/"+jfile) as json_file:
									data.append(json.load(json_file))
							filterData["featureVsActual"]=data
						elif k=="features4D":
							with open(pathPara+"/"+jsonFiles[0]) as json_file:
								data = json.load(json_file)
								filterData["plot4D"]=data
						elif k=="multipleCP":
							with open(pathPara+"/"+jsonFiles[0]) as json_file:
								data = json.load(json_file)
								filterData["multipleWhatIf"]=data

		# now append the filter data to the list
		res.append(filterData)
		
# write the resulting json
with open(pathData+'/finalJsons/result.json', 'w') as outfile:  
    json.dump(res, outfile)