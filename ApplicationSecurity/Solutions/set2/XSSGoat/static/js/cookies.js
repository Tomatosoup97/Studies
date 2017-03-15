
function encrypt(text, key) {
  var cipher = forge.cipher.createCipher('AES-GCM', key);
  cipher.start({iv: iv});
  cipher.update(forge.util.createBuffer(text));
  cipher.finish();
  var encrypted = cipher.output;
  return encrypted.toHex();
}

function setCookie(name, value) {
  const today = new Date();
  const expiry = new Date(today.getTime() + 30 * 24 * 3600 * 1000); // +30 days
  var expiration = "expires=" + expiry.toGMTString();
  document.cookie=name + "=" + escape(value) + "; path=/;" + expiration;
}

function setAndEncryptCookie(name, value) {
  return setCookie(name, encrypt(value, "{{ PUBLIC_KEY }}"));
}

function createCookies() {
  setAndEncryptCookie('confidential-info-encrypted',
                      $('#set-data')[0].text.value);
  setCookie('confidential-info',
            $('#set-data')[0].text.value);
  setCookie('is-pope', $('#set-data')[0].text.value);
  alert('Your settings have been saved');
}
