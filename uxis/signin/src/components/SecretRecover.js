import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import { SecretKey } from 'components/SecretKey'
import { Dialog } from 'components/Dialog'

const Actions = () => (
  <>
    <>&nbsp;</>
    <Button type="submit" intent={Intent.PRIMARY} large>Reset Password</Button>
  </>
)

export const SecretRecover = ({ client_id, code, access }) => (
  <Dialog
    icon="fa-key"
    title="Reset Password"
    url="/oauth2/reset"
    Actions={Actions}
  >
    <p className="bp3-ui-text bp3-running-text">
      Create a new password for <b>{access}</b>
    </p>
    <SecretKey />
    <input name="client_id" type="hidden" value={client_id} />
    <input name="token" type="hidden" value={code} />
  </Dialog>
)
