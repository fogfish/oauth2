import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button, Label, Input } from 'react-dress-code'
import { Link } from 'react-router-dom'
import { AccessKey } from 'components/AccessKey'

export const SecretReset = ({ client_id }) => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/reset" method="post">
        <DialogBody>
          <DialogTitle>
            <i className="fa fa-lock" aria-hidden="true"></i>
              &nbsp; &nbsp; Forgot Your Password ?
          </DialogTitle>
          <p className="dc-p">
            Type you <b>email</b> address to reset your password.
            We will send recovery instructions over email.
          </p>
          <AccessKey />

          <input name="client_id" type="hidden" value={client_id} />
        </DialogBody>
        <DialogActions with-link>
          <Link className="dc-link" to="/"><b>Sign In instead</b></Link>
          <Button primary type="submit">Reset Password</Button>
        </DialogActions>
      </form>
    </DialogContent>
  </Dialog>
)
