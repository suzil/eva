{-# LANGUAGE OverloadedStrings #-}

-- | AES-256-GCM encryption for credential secrets.
-- Wire format: [12-byte IV][16-byte auth tag][ciphertext]
-- Key derivation: SHA-256 of the raw key material (always 32 bytes).
module Eva.Crypto
  ( deriveKey
  , encrypt
  , decrypt
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEAD, AEADMode (..), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (throwCryptoError)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Derive a 32-byte AES-256 key from arbitrary key material using SHA-256.
deriveKey :: ByteString -> ByteString
deriveKey = convert . hashWith SHA256

-- | Encrypt plaintext with AES-256-GCM.
-- Output: [12-byte IV][16-byte auth tag][ciphertext]
encrypt :: ByteString -> ByteString -> IO ByteString
encrypt key plaintext = do
  iv <- getRandomBytes 12
  let cipher    = throwCryptoError (cipherInit key) :: AES256
      aead      = throwCryptoError (aeadInit AEAD_GCM cipher iv) :: AEAD AES256
      (tag, ct) = aeadSimpleEncrypt aead (mempty :: ByteString) plaintext 16
      AuthTag tagBytes = tag
  pure (iv <> convert tagBytes <> ct)

-- | Decrypt ciphertext produced by 'encrypt'.
-- Returns 'Left' on authentication failure or malformed input.
decrypt :: ByteString -> ByteString -> Either String ByteString
decrypt key blob
  | BS.length blob < 28 = Left "Ciphertext too short (minimum 28 bytes)"
  | otherwise =
      let (iv, rest)     = BS.splitAt 12 blob
          (tagBytes, ct) = BS.splitAt 16 rest
          cipher         = throwCryptoError (cipherInit key) :: AES256
          aead           = throwCryptoError (aeadInit AEAD_GCM cipher iv) :: AEAD AES256
          authTag        = AuthTag (convert tagBytes)
      in case aeadSimpleDecrypt aead (mempty :: ByteString) ct authTag of
           Nothing -> Left "GCM authentication tag verification failed"
           Just pt -> Right pt
