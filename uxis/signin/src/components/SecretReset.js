import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import { Link } from 'react-router-dom'
import { Dialog } from 'components/Dialog'
import { AccessKey } from 'components/AccessKey'

const Actions = () => (
  <>
    <Link className="bp3-button bp3-minimal bp3-intent-primary" to="/">
      <b>Sign In Instead</b>
    </Link>
    <Button type="submit" intent={Intent.PRIMARY} large>Reset Password</Button>
  </>
)

export const SecretReset = ({ client_id }) => (
  <Dialog
    icon="fa-lock"
    title="Forgot Your Password ?"
    url="/oauth2/reset"
    Actions={Actions}
  >
    <p className="bp3-ui-text bp3-running-text">
      Type you <b>email</b> address to reset your password.
      We will send recovery instructions over email.
    </p>
    <AccessKey />

    <input name="client_id" type="hidden" value={client_id} />
  </Dialog>
)
