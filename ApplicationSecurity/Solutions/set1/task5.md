# Digital signature using API

Using https://signrequest.com/

## Obtain authentication token:

`curl --user <email>:<password> --data 'subdomain=<created_subdomain>' https://signrequest.com/api/v1/api-tokens/`

_**Response:** Auth Token_

## Post document to the API

`curl -X POST -H 'Authorization: Token <auth-token>' -H 'Content-Type: multipart/form-data' -F 'file=@/<path>/document.pdf' -F 'external_id=<id_of_doc>' https://signrequest.com/api/v1/documents/`

_**Response:** document properties_

### Get "url" property from the response

## Send request to sign document

```
curl -X POST \
     -H 'Authorization: Token <auth-token>' \
     -H 'Content-Type:application/json' \
     -d '{ \
        "document": "https://signrequest.com/api/v1/documents/<obtained-url/", \
        "from_email": "contact@signrequest.com", \
        "message": "Please sign this document.", \
        "signers": [{"email": "your@email-address.com"}] \
      }' \
     https://signrequest.com/api/v1/signrequests/ \
```

_**Response:** Metadata and the desired url for signed document_
