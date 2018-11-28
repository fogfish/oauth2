import React from 'react'
import { Link } from 'react-dress-code'

export const GitHub = ({ client_id }) => (
  <Link href={`${window.env.GITHUB}&state=${client_id}`}>
    <i className="fa fa-github"></i> GitHub
  </Link>
)
