(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License version 2 as     *)
(*  published by the Free Software Foundation.                            *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA 02110-1301 USA.                                                    *)
(*                                                                        *)
(*  Special exception for linking this software with OpenSSL:             *)
(*                                                                        *)
(*  In addition, as a special exception, Skylable Ltd. gives permission   *)
(*  to link the code of this program with the OpenSSL library and         *)
(*  distribute linked combinations including the two. You must obey the   *)
(*  GNU General Public License in all respects for all of the code used   *)
(*  other than OpenSSL. You may extend this exception to your version     *)
(*  of the program, but you are not obligated to do so. If you do not     *)
(*  wish to do so, delete this exception statement from your version.     *)
(**************************************************************************)

type t =
  | NoError
  | AccessDenied
  | AccountProblem
  | AmbiguousGrantByEmailAddress
  | BadDigest
  | BucketAlreadyExists
  | BucketAlreadyOwnedByYou
  | BucketNotEmpty
  | CredentialsNotSupported
  | CrossLocationLoggingProhibited
  | EntityTooSmall
  | EntityTooLarge
  | ExpiredToken
  | IllegalVersioningConfigurationException
  | IncompleteBody
  | IncorrectNumberOfFilesInPostRequest
  | InlineDataTooLarge
  | InternalError
  | InvalidAccessKeyId
  | InvalidArgument
  | InvalidBucketName
  | InvalidBucketState
  | InvalidDigest
  | InvalidLocationConstraint
  | InvalidPart
  | InvalidPartOrder
  | InvalidPayer
  | InvalidPolicyDocument
  | InvalidRange
  | InvalidRequest
  | InvalidSecurity
  | InvalidSOAPRequest
  | InvalidStorageClass
  | InvalidTargetBucketForLogging
  | InvalidToken
  | InvalidURI
  | KeyTooLong
  | MalformedACLError
  | MalformedPOSTRequest
  | MalformedXML
  | MaxMessageLengthExceeded
  | MaxPostPreDataLengthExceededError
  | MetadataTooLarge
  | MethodNotAllowed
  | MissingContentLength
  | MissingRequestBodyError
  | MissingSecurityElement
  | MissingSecurityHeader
  | NoLoggingStatusForKey
  | NoSuchBucket
  | NoSuchKey
  | NoSuchLifecycleConfiguration
  | NoSuchUpload
  | NoSuchVersion
  | NotImplemented
  | NotSignedUp
  | NotSuchBucketPolicy
  | OperationAborted
  | PermanentRedirect
  | PreconditionFailed
  | Redirect
  | RequestIsNotMultiPartContent
  | RequestTimeout
  | RequestTimeTooSkewed
  | RequestTorrentOfBucketError
  | SignatureDoesNotMatch
  | ServiceUnavailable
  | SlowDown
  | TemporaryRedirect
  | TokenRefreshRequired
  | TooManyBuckets
  | UnexpectedContent
  | UnresolvableGrantByEmailAddress
  | UserKeyMustBeSpecified
  | RemoteServiceUnavailable
  | RemoteServiceTimeout

type details = (string * string) list
type headers = (string * string) list

exception ErrorReply of t * details * headers

let info = function
  | NoError ->
      "", "", `Ok
  | AccessDenied ->
      "AccessDenied",
      "Access Denied",
      `Forbidden
  | AccountProblem ->
      "AccountProblem",
      "There is a problem with your SX account. Please contact support",
      `Forbidden
  | AmbiguousGrantByEmailAddress ->
      "AmbiguousGrantByEmailAddress",
      "There is more than one account with this e-mail address.",
      `Bad_request
  | BadDigest ->
      "BadDigest",
      "Content-MD5 mismatch.",
      `Bad_request
  | BucketAlreadyExists ->
      "BucketAlreadyExists",
      "Bucket names are global, and the specified name is already in use. Please select a different name.",
      `Conflict
  | BucketAlreadyOwnedByYou ->
      "BucketAlreadyOwnedByYou",
      "The bucket is already owned by you.",
      `Conflict
  | BucketNotEmpty ->
      "BucketNotEmpty",
      "Cannot delete non-empty bucket.",
      `Conflict
  | CredentialsNotSupported ->
      "CredentialsNotSupported",
      "This request does not support credentials.",
      `Bad_request
  | CrossLocationLoggingProhibited ->
      "CrossLocationLoggingProhibited",
      "Logging is only allowed between buckets in the same location.",
      `Forbidden
  | EntityTooSmall ->
      "EntityTooSmall",
      "The object you tried to upload is smaller than the minimum allowed size.",
      `Bad_request
  | EntityTooLarge ->
      "EntityTooLarge",
      "The object you tried to upload exceeds the maximum allowed size.",
      `Bad_request
  | ExpiredToken ->
      "ExpiredToken",
      "The token has expired.",
      `Bad_request
  | IllegalVersioningConfigurationException ->
      "IllegalVersioningConfigurationException",
      "Invalid Versioning configuration in the request.",
      `Bad_request
  | IncompleteBody ->
      "IncompleteBody",
      "Request body too short according to the Content-Length HTTP header",
      `Bad_request
  | IncorrectNumberOfFilesInPostRequest ->
      "IncorrectNumberOfFilesInPostRequest",
      "POST requires exactly one file upload per request.",
      `Bad_request
  | InlineDataTooLarge ->
      "InlineDataTooLarge",
      "Inline data exceeds the maximum allowed size.",
      `Bad_request
  | InternalError ->
      "InternalError",
      "Please try again later: The server has encountered an internal error.",
      `Internal_server_error
  | InvalidAccessKeyId ->
      "InvalidAccessKeyId",
      "The SX Access Key Id you provided doesn't exist, make sure access_key is set to your SX username and secret_key to your SX auth token",
      `Forbidden
  | InvalidArgument ->
      "InvalidArgument",
      "Invalid Argument",
      `Bad_request
  | InvalidBucketName ->
      "InvalidBucketName",
      "The bucket name is invalid.",
      `Bad_request
  | InvalidBucketState ->
      "InvalidBucketState",
      "The current state of the bucket doesn't allow this request.",
      `Conflict
  | InvalidDigest ->
      "InvalidDigest",
      "The specified Content-MD5 is invalid.",
      `Bad_request
  | InvalidLocationConstraint ->
      "InvalidLocationConstraint",
      "Invalid location constraint. Please refer to the documentation", `Bad_request
  | InvalidPart ->
      "InvalidPart",
      "Some parts could not be found: they have not been uploaded or the entity tag doesn't match",
      `Bad_request
  | InvalidPartOrder ->
      "InvalidPartOrder",
      "Parts list must be in ascending order by part number.",
      `Bad_request
  | InvalidPayer ->
      "InvalidPayer",
      "Access to this object has been completely disabled.",
      `Forbidden
  | InvalidPolicyDocument ->
      "InvalidPolicyDocument",
      "The form does is not compliant with the policy document.",
      `Bad_request
  | InvalidRange ->
      "InvalidRange",
      "The requested byte range cannot be satisfied.",
      `Requested_range_not_satisfiable
  | InvalidRequest ->
      "InvalidRequest",
      "SOAP requests must be made over HTTPS.",
      `Bad_request
  | InvalidSecurity ->
      "InvalidSecurity",
      "Invalid security credentials.",
      `Forbidden
  | InvalidSOAPRequest ->
      "InvalidSOAPRequest",
      "Invalid SOAP request body.",
      `Bad_request
  | InvalidStorageClass ->
      "InvalidStorageClass",
      "Invalid storage class.",
      `Bad_request
  | InvalidTargetBucketForLogging ->
      "InvalidTargetBucketForLogging",
      "Log target bucket is missing, is not owned by you, or lacks appropriate grants for the group",
      `Bad_request
  | InvalidToken ->
      "InvalidToken",
      "Malformed token.", `Bad_request
  | InvalidURI ->
      "InvalidURI",
      "Failed to parse the specified URI.",
      `Bad_request
  | KeyTooLong ->
      "KeyTooLong",
      "Your access key is too long.",
      `Bad_request
  | MalformedACLError ->
      "MalformedACLError",
      "XML is not well-formed, or does not validate against the published schema.",
      `Bad_request
  | MalformedPOSTRequest ->
      "MalformedPOSTRequest",
      "Invalid multipart/form-data POST request body.",
      `Bad_request
  | MalformedXML ->
      "MalformedXML",
      "Configuration XML is not well-formed, or does not validate against the published schema.",
      `Bad_request
  | MaxMessageLengthExceeded ->
      "MaxMessageLengthExceeded",
      "The request was too big.",
      `Bad_request
  | MaxPostPreDataLengthExceededError ->
      "MaxPostPreDataLengthExceededError",
      "The POST request fields preceding the upload file were too large.",
      `Bad_request
  | MetadataTooLarge ->
      "MetadataTooLarge",
      "The metadata headers exceed the maximum allowed metadata size.",
      `Bad_request
  | MethodNotAllowed ->
      "MethodNotAllowed",
      "This resource doesn't allow the specified method.",
      `Method_not_allowed
  | MissingContentLength ->
      "MissingContentLength",
      "The Content-Length HTTP header is required.",
      `Length_required
  | MissingRequestBodyError ->
      "MissingRequestBodyError",
      "Request body XML is empty", `Bad_request
  | MissingSecurityElement ->
      "MissingSecurityElement",
      "The SOAP 1.1 request is missing a security element.",
      `Bad_request
  | MissingSecurityHeader ->
      "MissingSecurityHeader",
      "The request is missing a required header.",
      `Bad_request
  | NoLoggingStatusForKey ->
      "NoLoggingStatusForKey",
      "Keys don't have logging status sub-resources.",
      `Bad_request
  | NoSuchBucket ->
      "NoSuchBucket",
      "No such bucket.",
      `Not_found
  | NoSuchKey ->
      "NoSuchKey",
      "No such key.",
      `Not_found
  | NoSuchLifecycleConfiguration ->
      "NoSuchLifecycleConfiguration",
      "No such lifecycle configuration.",
      `Not_found
  | NoSuchUpload ->
      "NoSuchUpload",
      "No such multipart upload: the id might be invalid, already completed or aborted", `Not_found
  | NoSuchVersion ->
      "NoSuchVersion",
      "Version ID specified does not match any existing versions.",
      `Not_found
  | NotImplemented ->
      "NotImplemented",
      "The specified REST API is not implemented.",
      `Not_implemented
  | NotSignedUp ->
      "NotSignedUp",
      "Your account is not signed up for this service",
      `Forbidden
  | NotSuchBucketPolicy ->
      "NotSuchBucketPolicy",
      "The specified bucket does not have a policy.",
      `Not_found
  | OperationAborted ->
      "OperationAborted",
      "Please try again: a conflicting conditional operation is currently in progress against this resource.",
      `Conflict
  | PermanentRedirect ->
      "PermanentRedirect",
      "Please send all further request for this bucket to this new endpoint instead.",
      `Moved_permanently
  | PreconditionFailed ->
      "PreconditionFailed",
      "Some of the preconditions you specified didn't hold.",
      `Precondition_failed
  | Redirect ->
      "Redirect",
      "Temporary redirect.",
      `Temporary_redirect
  | RequestIsNotMultiPartContent ->
      "RequestIsNotMultiPartContent",
      "Bucket POST must be of the enclosure-type multipart/form-data.",
      `Bad_request
  | RequestTimeout ->
      "RequestTimeout",
      "The timeout period has elapsed while reading or writing to the server.",
      `Bad_request
  | RequestTimeTooSkewed ->
      "RequestTimeTooSkewed",
      "Check the clock on your machine: the difference to the server's time is too large.",
      `Forbidden
  | RequestTorrentOfBucketError ->
      "RequestTorrentOfBucketError",
      "Requesting the torrent file of a bucket is not permitted.",
      `Bad_request
  | SignatureDoesNotMatch ->
      "SignatureDoesNotMatch",
      "The request's signature doesn't match what we calculated. Please check that your S3 secret key matches your SX auth token and the signing method.", `Forbidden
  | ServiceUnavailable ->
      "ServiceUnavailable",
      "Please reduce your request rate.",
      `Service_unavailable
  | SlowDown ->
      "SlowDown",
      "Please reduce your request rate.",
      `Service_unavailable
  | TemporaryRedirect ->
      "TemporaryRedirect",
      "You are being redirected to the bucket while DNS updates.",
      `Temporary_redirect
  | TokenRefreshRequired ->
      "TokenRefreshRequired",
      "The provided token must be refreshed.",
      `Bad_request
  | TooManyBuckets ->
      "TooManyBuckets",
      "You are not allowed to create so many buckets.",
      `Bad_request
  | UnexpectedContent ->
      "UnexpectedContent",
      "This request does not support the specified content.",
      `Bad_request
  | UnresolvableGrantByEmailAddress ->
      "UnresolvableGrantByEmailAddress",
      "No account associated with the provided e-mail address.",
      `Bad_request
  | UserKeyMustBeSpecified ->
      "UserKeyMustBeSpecified",
      "The specified field name must be part of the bucket POST. Please check the order of fields.", `Bad_request
  | RemoteServiceUnavailable ->
      "ServiceUnavailable",
      "SX server is unavailable",
      `Bad_gateway
  | RemoteServiceTimeout ->
      "ServiceUnavailable",
      "SX server timed out",
      `Gateway_timeout
