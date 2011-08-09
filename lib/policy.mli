type effect = [ `Deny | `Allow ]

type aws_account_id = string
type canonical_user_id = string

type principals = [
| `AWS of aws_account_id list
| `CanonicalUserId of canonical_user_id list
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

val string_of_s3_policy : s3_policy -> string
