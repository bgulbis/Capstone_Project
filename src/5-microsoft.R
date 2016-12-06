# microsoft api

library(httr)

# Ocp-Apim-Subscription-Key
api <- "39b702f50fd04d1f9730243bbbd6e0e8"

url <- "https://api.projectoxford.ai/text/weblm/v1.0/generateNextWords"

# model: body
# words
# order: 1-5, optional
# maxNumOfCandidatesReturned, default = 5; optional

POST(url = url, config = list(add_headers(Ocp-Apim-Subscription-Key = api)),
     body = list(model = "body", words = "a case of"), encode = "json")
