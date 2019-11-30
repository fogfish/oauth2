import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import SecretKey from 'components/SecretKey'
import Dialog from 'components/Dialog'

const Actions = () => (
  <>
    <>&nbsp;</>
    <Button type="submit" intent={Intent.PRIMARY} large>Reset Password</Button>
  </>
)

const SecretRecover = ({ oauth2 }) => (
  <Dialog
    icon="fa-key"
    title="Reset Password"
    url="/oauth2/password"
    Actions={Actions}
  >
    <p className="bp3-ui-text bp3-running-text">
      Create a new password for&nbsp;
      <b>{oauth2.access}</b>
    </p>
    <SecretKey />
    <input name="response_type" type="hidden" value="password_recover" />
    <input name="client_id" type="hidden" value={oauth2.clientId} />
    <input name="state" type="hidden" value={oauth2.code} />
  </Dialog>
)

export default SecretRecover
