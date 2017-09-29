cat key-secret.lisp | openssl aes-256-cbc -k "$password" > key-secret.lisp.enc
cat ~/.ssh/nexmo1.key | openssl aes-256-cbc -k "$password" > nexmo1.key.enc
