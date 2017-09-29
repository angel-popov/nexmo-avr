cat ~/.config/nexmo/key-secret.lisp | openssl aes-256-cbc -k "ahduQu9ushou0Roh" > key-secret.lisp.enc
cat ~/.ssh/nexmo1.key | openssl aes-256-cbc -k "ahduQu9ushou0Roh" > nexmo1.key.enc
