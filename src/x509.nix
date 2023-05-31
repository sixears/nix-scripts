# -*- mode: shell-script; -*-

{ pkgs, ... }:

pkgs.writers.writeBashBin "x509" ''

# https://stackoverflow.com/questions/10175812/how-to-create-a-self-signed-certificate-with-openssl
# https://jamielinux.com/docs/openssl-certificate-authority/sign-server-and-client-certificates.html
# https://gist.github.com/Soarez/9688998
# https://devcenter.heroku.com/articles/ssl-certificate-self
# https://deliciousbrains.com/ssl-certificate-authority-for-local-https-development/

# -u: Treat unset variables and parameters other than the special parameters "@"
#     and "*" as an error when performing parameter expansion.  If expansion is
#     attempted on an unset variable or parameter, the shell prints an error
#     message, and, if not interactive, exits with a non-zero status.

# -o pipefail: If set, the return value of a pipeline is the value of the last
#              (rightmost) command to exit with a non-zero status, or zero if
#              all commands in the pipeline exit successfully.  This option is
#              disabled by default.

builtin set -u -o pipefail

# nullglob: If set, bash allows patterns which match no files to expand to a
#           null string, rather than themselves.
# dotglob:  If set, bash includes filenames beginning with a . in the results of
#           pathname expansion.
builtin shopt -s nullglob
builtin shopt -s dotglob
builtin shopt -s extglob

basename=${pkgs.coreutils}/bin/basename
false=${pkgs.coreutils}/bin/false
getopt=${pkgs.utillinux}/bin/getopt
true=${pkgs.coreutils}/bin/true

progname="$($basename "$0")"
verbose=$false
debug=$false
dry_run=$false

# ------------------------------------------------------------------------------

warn () {
  builtin echo -e "$1" 1>&2
}

info () {
  if $verbose; then
    builtin echo -e "$1" 1>&2
  fi
}

debug () {
  if $debug; then
    builtin echo -e "$1" 1>&2
  fi
}

usage () {
  usage="$(cat <<EOF
usage: $progname OPTION* FLACFILE+

do x509 stuff

options:

 -v | --verbose
 -n | --dry-run
 --help
 --debug
EOF
)"
  die 2 "$usage"
}

die() {
  warn "$2"
  exit $1
}

drdie() {
  warn "$2"
  if ! $dry_run; then
    exit $1
  fi
}

go() {
  exit="$1"; shift
  if $dry_run; then info "(CMD) $*"; else info "CMD> $*"; fi
  $dry_run || "$@" || die "$exit" "failed: $*"
}

goeval() {
  exit="$1"; shift
  if $dry_run; then
    info "(CMD) $*"
  else
    info "CMD> $*"
    eval "$*" || die "$exit" "failed: $*"
  fi
}

go() {
  exit="$1"; shift
  local cmd
  cmd="$(showcmd "$@")"
  if $dry_run; then info "(CMD) $cmd"; else info "CMD> $cmd"; fi
  $dry_run || "$@" || die "$exit" "failed: $*"
}

showcmd() {
  for i in "''${@:1:$(($#-1))}"; do
    builtin printf '%q ' "$i"
  done
  builtin printf '%q\n' "''${@:$(($#))}"
}

# ------------------------------------------------------------------------------

cat="${pkgs.coreutils}"/bin/cat
domainname="${pkgs.nettools}"/bin/domainname
hostname="${pkgs.inetutils}"/bin/hostname
openssl="${pkgs.openssl}"/bin/openssl

keyname="$($hostname -s)"."$(domainname)"

csr_conf="''$($cat <<EOF
# The main section is named req because the command we are using is req
# (openssl req ...)
[ req ]
# This specifies the default key size in bits. If not specified then 512 is
# used. It is used if the -new option is used. It can be overridden by using
# the -newkey option.
default_bits = 2048

# This is the default filename to write a private key to. If not specified the
# key is written to standard output. This can be overridden by the -keyout
# option.
# default_keyfile = oats.key

# If this is set to no then if a private key is generated it is not encrypted.
# This is equivalent to the -nodes command line option. For compatibility
# encrypt_rsa_key is an equivalent option.
encrypt_key = no

# This option specifies the digest algorithm to use. Possible values include
# md5 sha1 mdc2. If not present then MD5 is used. This option can be overridden
# on the command line.
default_md = sha256

# if set to the value no this disables prompting of certificate fields and just
# takes values from the config file directly. It also changes the expected
# format of the distinguished_name and attributes sections.
prompt = no

# if set to the value yes then field values to be interpreted as UTF8 strings,
# by default they are interpreted as ASCII. This means that the field values,
# whether prompted from a terminal or obtained from a configuration file, must
# be valid UTF8 strings.
utf8 = yes

# This specifies the section containing the distinguished name fields to
# prompt for when generating a certificate or certificate request.
distinguished_name = my_req_distinguished_name

# this specifies the configuration file section containing a list of extensions
# to add to the certificate request. It can be overridden by the -reqexts
# command line switch. See the x509v3_config(5) manual page for details of the
# extension section format.
req_extensions = my_extensions

[ my_req_distinguished_name ]
# C = XX
# ST = x
# L = xx
# O  = xx
CN = *.sixears.co.uk

[ my_extensions ]
basicConstraints=CA:FALSE
subjectAltName=@my_subject_alt_names
subjectKeyIdentifier = hash

[ my_subject_alt_names ]
DNS.1 = *.oats.org
DNS.2 = *.oats.net
DNS.3 = *.oats.in
DNS.4 = oats.org
DNS.5 = oats.net
DNS.6 = oats.in
EOF
)"

OPTS=$( $getopt --options vn                               \
                --longoptions debug,verbose,dry-run,help   \
                --name "$progname" -- "$@"                 )

[ $? -eq 0 ] || die 2 "options parsing failed (--help for help)"

# copy the values of OPTS (getopt quotes them) into the shell's $@
eval set -- "$OPTS"

while true; do
  case "$1" in
    -v | --verbose  ) verbose=$true ; shift   ;;
    -n | --dry-run  ) dry_run=$true ; shift   ;;
    --debug         ) debug=$true ; shift   ;;
    --help          ) usage                   ;;
    # !!! don't forget to update usage !!!
    -- ) shift; break ;;
    * ) break ;;
  esac
done

priv_key="''${privfn:-"$keyname".priv.key}"
pub_key="''${privfn:-"$keyname".pub.key}"
cert="''${privfn:-"$keyname".pem}"
csr_priv_key='csr.priv.key'
csr_out='csr.out'
ca_priv_key='ca.priv.key'
ca_cert='ca.pem'

mkconf() {
  extensions="$1"; shift

  local conf=""

  local -a conf=(
    "# The main section is named req because the command we are using is req"
    "# (openssl req ...)"
    "[ req ]"
    "# This specifies the default key size in bits. If not specified then 512 is"
    "# used. It is used if the -new option is used. It can be overridden by"
    "# using the -newkey option. "
    "default_bits = 2048"
    ""
    "# This is the default filename to write a private key to. If not specified"
    "# the key is written to standard output. This can be overridden by the"
    "# -keyout option."
    "# default_keyfile = oats.key"
    ""
    "# If this is set to no then if a private key is generated it is not."
    "# encrypted This is equivalent to the -nodes command line option. For"
    "# compatibility encrypt_rsa_key is an equivalent option. "
    "encrypt_key = no"
    ""
    "# This option specifies the digest algorithm to use. Possible values"
    "# include md5 sha1 mdc2. If not present then MD5 is used. This option can"
    "# be overridden on the command line."
    "default_md = sha256"
    ""
    "# if set to the value no this disables prompting of certificate fields and"
    "# just takes values from the config file directly. It also changes the"
    "# expected format of the distinguished_name and attributes sections."
    "prompt = no"
    ""
    "# if set to the value yes then field values to be interpreted as UTF8"
    "# strings, by default they are interpreted as ASCII. This means that the"
    "# field values, whether prompted from a terminal or obtained from a"
    "# configuration file, must be valid UTF8 strings."
    "utf8 = yes"
    ""
    "# This specifies the section containing the distinguished name fields to"
    "# prompt for when generating a certificate or certificate request."
    "distinguished_name = my_req_distinguished_name"
    ""
    "# this specifies the configuration file section containing a list of"
    "# extensions to add to the certificate request. It can be overridden by the"
    "# -reqexts command line switch. See the x509v3_config(5) manual page for"
    "# details of the extension section format."
    "$extensions = my_extensions"
    ""
    "[ my_req_distinguished_name ]"
    "# C = XX"
    "# ST = x"
    "# L = xx"
    "# O  = xx"
    "CN = *.sixears.co.uk"
    ""
    "[ my_extensions ]"
    "basicConstraints=CA:FALSE"
    "subjectKeyIdentifier = hash"
  )

  local i
  for i in "''${conf[@]}"; do
    builtin echo "$i"
  done

  local n=0
  if [ 0 -ne $# ]; then
    echo "subjectAltName=@my_subject_alt_names"
    echo ""
    echo "[ my_subject_alt_names ]"

    for i in "$@"; do
      builtin echo "DNS.$n = $i"
      n=$(($n+1))
    done
  fi
}

main() {
  if [ -e "$priv_key" ]; then
    drdie 3 "not overwriting '$priv_key'"
  fi

  if [ -e "$pub_key" ]; then
    drdie 3 "not overwriting '$pub_key'"
  fi

  # Generate a (private) RSA key to give to the CA (Certificate Authority)
  # Note that this private key should never be given to anyone else, *including
  # the certificate issuer*.
  go 3 $openssl genrsa -out "$priv_key" 2048
  # Optionally, the rsa public key can be extracted from the private key:
  go 4 $openssl rsa -in "$priv_key" -pubout -out "$pub_key"


  conf="$(mkconf "req_extensions")"
  debug 'CONFIGURATION:'
  debug "$conf"
  debug "----"

  # We now generate a Certificate Signing Request.
  go 5 $openssl req -new -keyout "$csr_priv_key" -out "$csr_out" \
                    -config <(builtin echo "$conf")
  builtin echo "to inspect the csr, run: "
  builtin echo "''$(showcmd $openssl req -in "$csr_out" -noout -text)"

  ## `openssl req -nodes -new -x509 -keyout xx -out xx`
  ## will generate a csr without a passphrase, with both priv & pub values in
  ## the same file
  ## -nodes is "no DES", that is, no DES-encryption (which is what requires the
  ## passphrase)

  # Generate a CA key for the subject.

  go 6 $openssl genrsa -out "$ca_priv_key" 2048

  # Generate a self signed certificate for the CA.

  conf="$(mkconf "x509_extensions")"
  debug 'CONFIGURATION:'
  debug "$conf"
  debug "----"

  go 7 $openssl req -new -x509 -key "$ca_priv_key" -out "$ca_cert" \
                    -config <(builtin echo "$conf")

  # One very easy way to sign a certificate is this...
  go 8 $openssl x509 -req -in "$csr_out" -CA "$ca_cert" -CAkey "$ca_priv_key" \
                          -CAcreateserial -out "$cert"
  builtin echo "to inspect the cert, run: "
  builtin echo "''$(showcmd $openssl x509 -in "$cert" -noout -text)"
}

case $# in
  0 ) main ;;
  * ) usage ;;
  * ) for i in "$@"; do main "$i"; done ;;
esac
''
