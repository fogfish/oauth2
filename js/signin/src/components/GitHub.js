import React from 'react'
import { AnchorButton, Intent } from '@blueprintjs/core'

export const GitHub = ({ client_id }) => (
  <AnchorButton
    intent={Intent.PRIMARY}
    minimal
    href={`${window.env.GITHUB}&state=${client_id}`}
  >
    <i className="fa fa-github"></i>&nbsp;&nbsp;GitHub
  </AnchorButton>
)
