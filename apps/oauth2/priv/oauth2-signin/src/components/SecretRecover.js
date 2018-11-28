import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button, Label, Input } from 'react-dress-code'
import { SecretKey } from 'components/SecretKey'

export const SecretRecover = ({ client_id, code, access }) => (
  <Dialog>
    <DialogContent>
      <form className="form-group" action="/oauth2/recover" method="post">
        <DialogBody>
          <DialogTitle>
            <i className="fa fa-key" aria-hidden="true"></i>
              &nbsp; &nbsp; Reset Password for <b>{access}</b>
          </DialogTitle>
          <p />
          <SecretKey />
          <input name="client_id" type="hidden" value={client_id} />
          <input name="token" type="hidden" value={code} />
        </DialogBody>
        <DialogActions>
          <Button primary type="submit">Reset Password</Button>
        </DialogActions>
      </form>
    </DialogContent>
  </Dialog>
)
