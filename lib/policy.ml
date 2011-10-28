type effect = [ `Deny | `Allow ]

type aws_account_id = string
type canonical_user_id = string

(* by trial-and-error, it appears that both kinds of id's cannot be
   mixed in the list of princiapals ... thanks Amazon! *)
type principals = [
| `AWS of aws_account_id list
| `CanonicalUserId of canonical_user_id list
| `Everyone
]

type ('a, 'b) statement = {
  statement_id: string;
  effect: effect;
  principals: principals;
  actions: 'a list;
  resources: 'b list
}

type ('a, 'b) policy = { 
  policy_id: string; 
  statements: ('a, 'b) statement list 
}

type s3_action = [
| `AbortMultipartUpload 
| `CreateBucket 
| `DeleteBucket 
| `DeleteObject
| `DeleteObjectVersion 
| `GetBucketAcl 
| `GetBucketLocation
| `GetBucketNotification 
| `GetBucketPolicy 
| `GetBucketRequestPayment
| `GetBucketVersioning 
| `GetObject 
| `GetObjectAcl 
| `GetObjectVersion
| `GetObjectVersionAcl 
| `ListAllMyBuckets 
| `ListBucket
| `ListBucketMultipartUploads 
| `ListBucketVersions
| `ListMultipartUploadParts 
| `PutBucketAcl 
| `PutBucketNotification
| `PutBucketPolicy 
| `PutBucketRequestPayment 
| `PutBucketVersioning
| `PutObject 
| `PutObjectAcl 
| `PutObjectAclVersion 
| `All
]

type s3_resource = string
type s3_policy = (s3_action, s3_resource) policy

open Yojson

let string_of_s3_action = function
| `AbortMultipartUpload ->        "s3:AbortMultipartUpload"
| `CreateBucket ->                "s3:CreateBucket"
| `DeleteBucket ->                "s3:DeleteBucket"
| `DeleteObject->                 "s3:DeleteObject"
| `DeleteObjectVersion ->         "s3:DeleteObjectVersion"
| `GetBucketAcl ->                "s3:GetBucketAcl"
| `GetBucketLocation->            "s3:GetBucketLocation"
| `GetBucketNotification ->       "s3:GetBucketNotification"
| `GetBucketPolicy ->             "s3:GetBucketPolicy"
| `GetBucketRequestPayment->      "s3:GetBucketRequestPayment"
| `GetBucketVersioning ->         "s3:GetBucketVersioning"
| `GetObject ->                   "s3:GetObject"
| `GetObjectAcl ->                "s3:GetObjectAcl"
| `GetObjectVersion->             "s3:GetObjectVersion"
| `GetObjectVersionAcl ->         "s3:GetObjectVersionAcl"
| `ListAllMyBuckets ->            "s3:ListAllMyBuckets"
| `ListBucket->                   "s3:ListBucket"
| `ListBucketMultipartUploads ->  "s3:ListBucketMultipartUploads"
| `ListBucketVersions->           "s3:ListBucketVersions"
| `ListMultipartUploadParts ->    "s3:ListMultipartUploadParts"
| `PutBucketAcl ->                "s3:PutBucketAcl"
| `PutBucketNotification->        "s3:PutBucketNotification"
| `PutBucketPolicy ->             "s3:PutBucketPolicy"
| `PutBucketRequestPayment ->     "s3:PutBucketRequestPayment"
| `PutBucketVersioning->          "s3:PutBucketVersioning"
| `PutObject ->                   "s3:PutObject"
| `PutObjectAcl ->                "s3:PutObjectAcl"
| `PutObjectAclVersion ->         "s3:PutObjectAclVersion"
| `All->                          "s3:*"


let rec string_of_s3_policy s3_policy =
  let j = json_of_s3_policy s3_policy in
  Yojson.Basic.to_string j

and json_of_s3_policy s3_policy =
  `Assoc [
    "Id", `String s3_policy.policy_id;
    "Statement", `List (List.map json_of_s3_statement s3_policy.statements);
  ]
and json_of_s3_statement s3_statement =
  `Assoc [
    "Sid",      `String s3_statement.statement_id;
    "Resource", `List (List.map json_of_s3_resource s3_statement.resources);
    "Action",   `List (List.map json_of_s3_action s3_statement.actions);
    "Effect",    json_of_effect s3_statement.effect;
    "Principal", json_of_principals s3_statement.principals;
  ]

and json_of_effect = function
  | `Allow -> `String "Allow"
  | `Deny -> `String "Deny"

and json_of_principals = function
  | `AWS account_ids ->
      `Assoc [
        "AWS", `List (List.map (fun aid -> `String (string_of_account_id aid)) account_ids)
      ]
  | `CanonicalUserId canonical_user_ids ->
      `Assoc [
        "CanonicalUser", `List (List.map (fun cid -> `String cid) canonical_user_ids)
      ]
  | `Everyone ->
      `Assoc ["AWS", `String "*"]

and json_of_s3_action action =
  `String (string_of_s3_action action)

and json_of_s3_resource resource =
  `String (string_of_s3_resource resource)

and string_of_s3_resource resource =
  "arn:aws:s3:::" ^ resource
  
and string_of_account_id account_id =
  "arn:aws:iam::" ^ account_id ^ ":root"

